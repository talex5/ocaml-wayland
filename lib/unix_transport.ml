open Lwt.Syntax

let xdg_runtime_dir () =
  match Sys.getenv_opt "XDG_RUNTIME_DIR" with
  | Some x -> x
  | None -> Fmt.failwith "$XDG_RUNTIME_DIR not set and $WAYLAND_DISPLAY is not an absolute path"

let make_display_absolute x =
  if Filename.is_implicit x then
    Filename.concat (xdg_runtime_dir ()) x
  else
    x

let of_socket socket = object (_ : S.transport)
  method send data fds =
    let v = Lwt_unix.IO_vectors.create () in
    let { Cstruct.buffer; off; len } = data in
    Lwt_unix.IO_vectors.append_bigarray v buffer off len;
    let rec send_all ~fds =
      let* sent = Lwt_unix.send_msg ~socket ~io_vectors:v ~fds in
      Lwt_unix.IO_vectors.drop v sent;
      if Lwt_unix.IO_vectors.is_empty v then Lwt.return_unit
      else send_all ~fds:[]
    in
    send_all ~fds

  method recv io_vectors =
    Lwt_unix.recv_msg ~socket ~io_vectors
end

let connect () =
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
    Lwt.return (of_socket socket)
