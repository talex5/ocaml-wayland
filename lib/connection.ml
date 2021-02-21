open Lwt.Syntax
open Internal

type t = Internal.connection

let connect role transport handler =
    let t = {
      transport;
      role;
      objects = Objects.empty;
      free_ids = [];
      next_id = 2l;
      incoming_fds = Queue.create ();
      outbox = Queue.create ();
    } in
    let display_proxy = Proxy.add_root t handler in
    (t, display_proxy)

(* Dispatch all complete messages in [recv_buffer]. *)
let rec process_recv_buffer t recv_buffer =
  match Msg.parse ~fds:t.incoming_fds (Recv_buffer.data recv_buffer) with
  | None -> ()
  | Some msg ->
    begin
      let obj = Msg.obj msg in
      match Objects.find_opt obj t.objects with
      | None -> Fmt.failwith "No such object %ld" obj
      | Some (Generic proxy) ->
        Log.info (fun f ->
            let (module M) = proxy.handler.metadata in
            let msg_name, arg_info =
              match t.role with
              | `Client -> M.events (Msg.op msg)
              | `Server -> M.requests (Msg.op msg)
            in
            f "@[<h><- %a.%s %a@]"
              pp_proxy proxy
              msg_name
              (Msg.pp_args arg_info) msg);
        proxy.handler.dispatch proxy (Msg.cast msg)
    end;
    Recv_buffer.update_consumer recv_buffer (Msg.length msg);
    (* Fmt.pr "Buffer after dispatch: %a@." Recv_buffer.dump recv_buffer; *)
    process_recv_buffer t recv_buffer

let listen t =
  let recv_buffer = Recv_buffer.create 4096 in
  let rec aux () =
    let* (got, fds) = t.transport#recv (Recv_buffer.free_buffer recv_buffer) in
    List.iter (fun fd -> Queue.add fd t.incoming_fds) fds;
    if got = 0 then (
      Log.info (fun f -> f "Got end-of-file on wayland connection");
      Lwt.return_unit
    ) else (
      Recv_buffer.update_producer recv_buffer got;
      Log.debug (fun f -> f "Ring after adding %d bytes: %a@." got Recv_buffer.dump recv_buffer);
      process_recv_buffer t recv_buffer;
      aux ()
    )
  in
  aux ()
