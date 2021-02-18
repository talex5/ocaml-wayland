open Lwt.Syntax
open Internal

type t = Internal.connection

let xdg_runtime_dir () =
  match Sys.getenv_opt "XDG_RUNTIME_DIR" with
  | Some x -> x
  | None -> Fmt.failwith "$XDG_RUNTIME_DIR not set and $WAYLAND_DISPLAY is not an absolute path"

let make_display_absolute x =
  if Filename.is_implicit x then
    Filename.concat (xdg_runtime_dir ()) x
  else
    x

let connect handler =
  match Sys.getenv_opt "WAYLAND_SOCKET" with
  | Some _ -> failwith "TODO: WAYLAND_SOCKET"   (* TODO *)
  | None ->
    let display =
      Sys.getenv_opt "WAYLAND_DISPLAY"
      |> Option.value ~default:"wayland-0"
      |> make_display_absolute
    in
    let socket =
      Unix.(socket PF_UNIX SOCK_STREAM 0 ~cloexec:true)
      |> Lwt_unix.of_unix_file_descr
    in
    Log.info (fun f -> f "Connecting to %S" display);
    let* () = Lwt_unix.connect socket (Unix.ADDR_UNIX display) in
    let t = {
      socket;
      role = `Client;
      objects = Objects.empty;
      free_ids = [];
      next_id = 2l;
      incoming_fds = Queue.create ();
      outbox = Queue.create ();
    } in
    let display_proxy = Proxy.add_root t handler in
    Lwt.return (t, display_proxy)

(* Dispatch all complete messages in [recv_buffer]. *)
let rec process_recv_buffer t recv_buffer =
  match Msg.parse ~fds:t.incoming_fds (Recv_buffer.data recv_buffer) with
  | None -> ()
  | Some msg ->
    begin
      let obj = Msg.obj msg in
      match Objects.find_opt obj t.objects with
      | None -> Fmt.failwith "No such object %ld" obj
      | Some (Handler (proxy, handler)) ->
        Log.info (fun f ->
            let (module M) = proxy.metadata in
            let msg_name, arg_info =
              match t.role with
              | `Client -> M.events (Msg.op msg)
              | `Server -> M.requests (Msg.op msg)
            in
            f "@[<h><- %a.%s %a@]"
              pp_proxy proxy
              msg_name
              (Msg.pp_args arg_info) msg);
        handler proxy (Msg.cast msg)
    end;
    Recv_buffer.update_consumer recv_buffer (Msg.length msg);
    (* Fmt.pr "Buffer after dispatch: %a@." Recv_buffer.dump recv_buffer; *)
    process_recv_buffer t recv_buffer

let listen t =
  let recv_buffer = Recv_buffer.create 4096 in
  let rec aux () =
    let io_vectors = Recv_buffer.io_vec recv_buffer in
    let* (got, fds) = Lwt_unix.recv_msg ~socket:t.socket ~io_vectors in
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
