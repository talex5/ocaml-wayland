open Eio.Std
open Internal

type 'a t = 'a connection
let invalid_object t message = error t ~object_id:1l ~code:0l ~message [@@inline never]
let invalid_message t message = error t ~object_id:1l ~code:1l ~message [@@inline never]
let[@inline never] bad_msg_len t len op is_server =
  let error = Format.asprintf "Invalid message length %u (op=%d)" len op in
  if is_server then (
    Log.warn (fun f -> f "Bad message from client: %s" error);
    invalid_message t error
  ) else (
    Log.err (fun f -> f "Bad message from server: %s" error)
  )

let process_msg is_server msg t =
  let obj = Msg.obj msg in
  let succeeded = ref false in
  begin match Objects.find obj t.objects with
  | exception Not_found ->
    let message = Format.asprintf "No such object %lu (op=%d)" obj (Msg.op msg) in
    if is_server then (
      Internal.error t ~object_id:1l (* wl_display *) ~code:0l (* invalid_object *) ~message;
      Log.warn (fun f -> f "Bad message from client: %s" message)
    ) else (
      Log.err (fun f -> f "Bad message from server: %s" message)
    )
  | Generic proxy ->
    let msg = Msg.cast msg in
    t.trace.inbound proxy msg;
    if proxy.can_recv then (
      (* This is where the desires of wayland-proxy-virtwl clash with libwayland behavior.
         libwayland doesn't support delaying object destruction. *)
      try
        proxy.handler#dispatch proxy msg;
        succeeded := true
      with
      | Msg.Error { object_id; code; message } when is_server ->
        Log.warn (fun f -> f "Protocol error handling incoming message for %a: code %d, message %s"
                             pp_proxy proxy (Int32.to_int code) message);
        Internal.error t ~object_id ~code ~message
      | Msg.Error { object_id = 1l; code = 0l; message } ->
        (* Malformed message sent by server *)
        Log.err (fun f -> f "Malformed event sent by server: %s" message);
      | ex ->
        let bt = Printexc.get_raw_backtrace () in
        Log.err (fun f -> f "Uncaught exception handling incoming message for %a:@,%a"
                            pp_proxy proxy Fmt.exn_backtrace (ex, bt));
        begin match ex with
        | Out_of_memory | Stack_overflow -> (
          (if is_server then (
             Internal.error t ~object_id:1l ~code:2l ~message:"Out of memory"));
          raise ex (* System is now in undefined state, exit! *))
        | _ ->
          Internal.error t ~object_id:1l ~code:3l ~message:"Uncaught OCaml exception";
          raise ex
        end
    ) else if is_server then (
      Log.warn (fun f -> f "Client sent message to zombie object %a" pp_proxy proxy);
      invalid_object t (Format.asprintf "Message sent to already-destroyed object %a" pp_proxy proxy)
    ) else (
      Log.err (fun f -> f "Server sent message to object %a it had already destroyed!" pp_proxy proxy)
    )
  end;
  !succeeded

(** Dispatch all complete messages in [recv_buffer].
    Return [true] if everything was okay and [false] to shut down the connection. *)
let rec process_recv_buffer is_server t recv_buffer =
  match Msg.parse ~fds:t.incoming_fds (Recv_buffer.data recv_buffer) with
  | None -> true
  | Some msg ->
    let len = Msg.length msg in
    if len < 8 then (
      (* Too short for even a proper error message! *)
      bad_msg_len t len 0 is_server;
      false
    ) else if len > 4096 || (len land 3) <> 0 then (
      bad_msg_len t len (Msg.op msg) is_server;
      false
    ) else (
      let keep_going = process_msg is_server msg t in
      Recv_buffer.update_consumer recv_buffer len;
      (* Unix.sleepf 0.001; *)
      (* Fmt.pr "Buffer after dispatch: %a@." Recv_buffer.dump recv_buffer; *)
      keep_going && process_recv_buffer is_server t recv_buffer
    )

let stop = Internal.shutdown

let listen t =
  Switch.run @@ fun sw ->
  let recv_buffer = Recv_buffer.create 4096 in
  let is_server = match t.role with
    | `Server -> true
    | `Client -> false
  in
  let rec aux () =
    let (got, fds) = t.transport#recv ~sw (Recv_buffer.free_buffer recv_buffer) in
    List.iter (fun fd -> Queue.add (Eio_unix.Fd.remove fd |> Option.get) t.incoming_fds) fds;
    if got = 0 then (
      Log.info (fun f -> f "Got end-of-file on wayland connection");
      stop t
    ) else (
      Recv_buffer.update_producer recv_buffer got;
      Log.debug (fun f -> f "Ring after adding %d bytes: %a" got Recv_buffer.dump recv_buffer);
      if process_recv_buffer is_server t recv_buffer then
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
