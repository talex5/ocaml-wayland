open Eio.Std
open Internal

type 'a t = 'a Internal.connection

(* Dispatch all complete messages in [recv_buffer]. *)
let rec process_recv_buffer t recv_buffer =
  match Msg.parse ~fds:t.incoming_fds (Recv_buffer.data recv_buffer) with
  | None -> ()
  | Some msg ->
    begin
      let obj = Msg.obj msg in
      match Objects.find_opt obj t.objects with
      | None -> Fmt.failwith "No such object %lu (op=%d)" obj (Msg.op msg);
      | Some (Generic proxy) ->
        let msg = Msg.cast msg in
        t.trace.inbound proxy msg;
        if proxy.can_recv then (
          try
            proxy.handler#dispatch proxy msg
          with ex ->
            let bt = Printexc.get_raw_backtrace () in
            Log.err (fun f -> f "Uncaught exception handling incoming message for %a:@,%a"
                        pp_proxy proxy Fmt.exn_backtrace (ex, bt))
        ) else (
          Fmt.failwith "Received message for %a, which was shut down!" pp_proxy proxy
        )
    end;
    Recv_buffer.update_consumer recv_buffer (Msg.length msg);
    (* Unix.sleepf 0.001; *)
    (* Fmt.pr "Buffer after dispatch: %a@." Recv_buffer.dump recv_buffer; *)
    process_recv_buffer t recv_buffer

let listen t =
  Switch.run @@ fun sw ->
  let recv_buffer = Recv_buffer.create 4096 in
  let rec aux () =
    let (got, fds) = t.transport#recv ~sw (Recv_buffer.free_buffer recv_buffer) in
    List.iter (fun fd -> Queue.add (Eio_unix.Fd.remove fd |> Option.get) t.incoming_fds) fds;
    if got = 0 then (
      Log.info (fun f -> f "Got end-of-file on wayland connection");
      t.transport#shutdown
    ) else (
      Recv_buffer.update_producer recv_buffer got;
      Log.debug (fun f -> f "Ring after adding %d bytes: %a" got Recv_buffer.dump recv_buffer);
      process_recv_buffer t recv_buffer;
      aux ()
    )
  in
  try aux ()
  with Eio.Io _ as ex when not t.transport#up ->
    Log.warn (fun f -> f "Listen error (but connection already closed): %a" Fmt.exn ex)

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
    )

let connect ~sw ~trace role transport handler =
  let incoming_fds = Queue.create () in
  Switch.on_release sw (fun () -> Queue.iter Unix.close incoming_fds);
  let t = {
    sw;
    transport = (transport :> S.transport);
    unpause = ignore;
    role;
    objects = Objects.empty;
    free_ids = [];
    next_id = (match role with `Client -> 2l | `Server -> 0xff000000l);
    incoming_fds;
    outbox = Queue.create ();
    trace = Proxy.trace trace;
    display_proxy = None;
  } in
  let display_proxy = Proxy.add_root t (handler :> _ Proxy.Handler.t) in
  Eio.Fiber.fork ~sw (fun () ->
      Fun.protect (fun () -> listen t)
        ~finally:(fun () -> clean_up t)
    );
  (t, display_proxy)

let stop t =
  t.transport#shutdown

let dump f t =
  let pp_item f (_id, Generic proxy) = pp_proxy f proxy in
  Fmt.pf f "@[<v2>Connection on %t with %d objects:@,%a@]"
    t.transport#pp
    (Objects.cardinal t.objects)
    (Fmt.Dump.list pp_item) (Objects.bindings t.objects)
