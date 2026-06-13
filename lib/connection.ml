open Eio.Std
open Internal

type 'a t = 'a Internal.connection

(** Dispatch all complete messages in [recv_buffer].
    Return [true] if everything was okay and [false] to shut down the connection. *)
let rec process_recv_buffer t recv_buffer =
  match Msg.parse ~fds:t.incoming_fds (Recv_buffer.data recv_buffer) with
  | None -> true
  | Some msg ->
     let keep_going =
       let is_server = match t.role with
         | `Server -> true
         | `Client -> false
       in
       let obj = Msg.obj msg in
       match Objects.find_opt obj t.objects with
       | None ->
          let message = Format.asprintf "No such object %lu (op=%d)" obj (Msg.op msg) in
          (if is_server then Internal.error t ~id:1l (* wl_display *) ~code:0l (* invalid_object *) ~message);
          false
       | Some (Generic proxy) ->
          let msg = Msg.cast msg in
          t.trace.inbound proxy msg;
          if proxy.can_recv then (
            try
              proxy.handler#dispatch proxy msg;
              true
            with
            | Proxy.Error { id; code; message } when is_server ->
               Log.warn (fun f -> f "Protocol error handling incoming message for %a: code %d, message %s"
                         pp_proxy proxy (Int32.to_int code) message);
               Internal.error t ~id ~code ~message;
               false
            | ex ->
               let bt = Printexc.get_raw_backtrace () in
               Log.err (fun f -> f "Uncaught exception handling incoming message for %a:@,%a"
                                   pp_proxy proxy Fmt.exn_backtrace (ex, bt));
               if is_server then (
                 match ex with
                 | Out_of_memory | Stack_overflow ->
                    Internal.error t ~id:1l ~code:2l ~message:"Out of memory";
                    raise ex (* System is now in undefined state, exit! *)
                 | _ -> Internal.error t ~id:1l ~code:3l ~message:"Uncaught OCaml exception"
               );
               false
          ) else (
            Fmt.failwith "Received message for %a, which was shut down!" pp_proxy proxy
          )
     in
     Recv_buffer.update_consumer recv_buffer (Msg.length msg);
     (* Unix.sleepf 0.001; *)
     (* Fmt.pr "Buffer after dispatch: %a@." Recv_buffer.dump recv_buffer; *)
     if keep_going then
       process_recv_buffer t recv_buffer
     else
       false

let stop = Internal.shutdown

let listen t =
  Switch.run @@ fun sw ->
  let recv_buffer = Recv_buffer.create 4096 in
  let rec aux () =
    let (got, fds) = t.transport#recv ~sw (Recv_buffer.free_buffer recv_buffer) in
    List.iter (fun fd -> Queue.add (Eio_unix.Fd.remove fd |> Option.get) t.incoming_fds) fds;
    if got = 0 then (
      Log.info (fun f -> f "Got end-of-file on wayland connection");
      stop t
    ) else (
      Recv_buffer.update_producer recv_buffer got;
      Log.debug (fun f -> f "Ring after adding %d bytes: %a" got Recv_buffer.dump recv_buffer);
      if process_recv_buffer t recv_buffer then
        aux ()
      else (
        Log.err (fun f -> f "Connection closed unexpectedly");
        stop t
      )
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
  let (promise, resolver) = Eio.Promise.create () in
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
    error = No_error;
    promise;
    resolver;
  } in
  let display_proxy = Proxy.add_root t (handler :> _ Proxy.Handler.t) in
  t.display_proxy <- Some display_proxy;
  Eio.Fiber.fork ~sw (fun () ->
      Fun.protect (fun () -> listen t)
        ~finally:(fun () -> clean_up t)
    );
  (t, display_proxy)

let error (t: [`Server] t) = Internal.error t

let dump f t =
  let pp_item f (_id, Generic proxy) = pp_proxy f proxy in
  Fmt.pf f "@[<v2>Connection on %t with %d objects:@,%a@]"
    t.transport#pp
    (Objects.cardinal t.objects)
    (Fmt.Dump.list pp_item) (Objects.bindings t.objects)
