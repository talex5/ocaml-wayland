open Lwt.Syntax
open Internal

type 'a t = 'a Internal.connection

(* Dispatch all complete messages in [recv_buffer]. *)
let rec process_recv_buffer t recv_buffer =
  let* () = t.paused in
  match Msg.parse ~fds:t.incoming_fds (Recv_buffer.data recv_buffer) with
  | None -> Lwt.return_unit
  | Some msg ->
    begin
      let obj = Msg.obj msg in
      match Objects.find_opt obj t.objects with
      | None -> Fmt.failwith "No such object %ld" obj
      | Some (Generic proxy) ->
        let msg = Msg.cast msg in
        t.trace.inbound proxy msg;
        if proxy.can_recv then
          proxy.handler#dispatch proxy msg
        else
          Fmt.failwith "Received message for %a, which was shut down!" pp_proxy proxy
    end;
    Recv_buffer.update_consumer recv_buffer (Msg.length msg);
    (* Unix.sleepf 0.001; *)
    (* Fmt.pr "Buffer after dispatch: %a@." Recv_buffer.dump recv_buffer; *)
    process_recv_buffer t recv_buffer

let listen t =
  let recv_buffer = Recv_buffer.create 4096 in
  let rec aux () =
    let* (got, fds) = t.transport#recv (Recv_buffer.free_buffer recv_buffer) in
    if Lwt.is_sleeping t.closed then (
      List.iter (fun fd -> Queue.add fd t.incoming_fds) fds;
      if got = 0 then (
        Log.info (fun f -> f "Got end-of-file on wayland connection");
        Lwt.return_unit
      ) else (
        Recv_buffer.update_producer recv_buffer got;
        Log.debug (fun f -> f "Ring after adding %d bytes: %a" got Recv_buffer.dump recv_buffer);
        let* () = process_recv_buffer t recv_buffer in
        aux ()
      )
    ) else (
      List.iter Unix.close fds;
      failwith "Connection is closed"
    )
  in
  Lwt.try_bind aux
    (fun () ->
       if Lwt.is_sleeping t.closed then Lwt.wakeup t.set_closed (Ok ());
       Queue.iter Unix.close t.incoming_fds;
       Lwt.return_unit;
    )
    (fun ex ->
       if Lwt.is_sleeping t.closed then
         Lwt.wakeup t.set_closed (Error ex)
       else
         Log.debug (fun f -> f "Listen error (but connection already closed): %a" Fmt.exn ex);
       Queue.iter Unix.close t.incoming_fds;
       Lwt.return_unit;
    )

let clean_up t =
  t.objects |> Objects.iter (fun _ (Generic obj) ->
      obj.on_delete |> Queue.iter (fun f ->
          try f ()
          with ex ->
            Log.warn (fun f -> f "Error from %a's on_delete handler called at end-of-connection: %a"
                         pp_proxy obj
                         Fmt.exn ex)
        );
      Queue.clear obj.on_delete
    );
  Lwt.return_unit

let connect ~trace role transport handler =
  let closed, set_closed = Lwt.wait () in
  let t = {
    transport = (transport :> S.transport);
    paused = Lwt.return_unit;
    unpause = ignore;
    role;
    objects = Objects.empty;
    free_ids = [];
    next_id = (match role with `Client -> 2l | `Server -> 0xff000000l);
    incoming_fds = Queue.create ();
    outbox = Queue.create ();
    closed;
    set_closed;
    trace = Proxy.trace trace;
  } in
  let display_proxy = Proxy.add_root t (handler :> _ Proxy.Handler.t) in
  Lwt.async (fun () ->
      Lwt.finalize
        (fun () -> listen t)
        (fun () -> clean_up t)
    );
  (t, display_proxy)

let closed t = t.closed

let set_paused t = function
  | false ->
    if Lwt.is_sleeping t.paused then t.unpause ()
  | true ->
    if not (Lwt.is_sleeping t.paused) then (
      let paused, set_paused = Lwt.wait () in
      t.paused <- paused;
      t.unpause <- Lwt.wakeup set_paused
    )
