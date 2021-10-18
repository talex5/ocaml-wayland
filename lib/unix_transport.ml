open Lwt.Syntax

(* SIGPIPE makes no sense in a modern application. *)
let () = Sys.(set_signal sigpipe Signal_ignore)

type t = <
  S.transport;
  close : unit Lwt.t;
  socket : Lwt_unix.file_descr
>

let xdg_runtime_dir () =
  match Sys.getenv_opt "XDG_RUNTIME_DIR" with
  | Some x -> x
  | None -> Fmt.failwith "$XDG_RUNTIME_DIR not set and $WAYLAND_DISPLAY is not an absolute path"

let make_display_absolute x =
  if Filename.is_implicit x then
    Filename.concat (xdg_runtime_dir ()) x
  else
    x

let of_socket socket = object (_ : #S.transport)
  val mutable up = true

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

  method recv { Cstruct.buffer; off; len } =
    let io_vectors = Lwt_unix.IO_vectors.create () in
    Lwt_unix.IO_vectors.append_bigarray io_vectors buffer off len;
    Lwt.catch
      (fun () ->
         let+ (got, fds) = Lwt_unix.recv_msg ~socket ~io_vectors in
         if got = 0 then up <- false;
         (got, fds)
      )
      (function
        | Unix.Unix_error(Unix.ECONNRESET, _, _) ->
          up <- false;
          Lwt.return (0, [])
        | ex -> Lwt.fail ex
      )

  method shutdown =
    up <- false;
    Lwt_unix.shutdown socket Lwt_unix.SHUTDOWN_SEND;
    Lwt.return_unit

  method up = up

  method socket = socket

  method close =
    up <- false;
    Lwt_unix.close socket

  method pp f =
    let fd : int = Obj.magic (Lwt_unix.unix_file_descr socket) in
    Fmt.pf f "%d" fd
end

let socket_path ?wayland_display () =
  let wayland_display =
    match wayland_display with
    | Some x -> x
    | None -> 
      Sys.getenv_opt "WAYLAND_DISPLAY"
      |> Option.value ~default:"wayland-0"
  in
  make_display_absolute wayland_display

let connect () =
  match Sys.getenv_opt "WAYLAND_SOCKET" with
  | None | Some "" ->
    let display = socket_path () in
    let socket =
      Unix.(socket PF_UNIX SOCK_STREAM 0 ~cloexec:true)
      |> Lwt_unix.of_unix_file_descr
    in
    Log.info (fun f -> f "Connecting to %S" display);
    let* () = Lwt_unix.connect socket (Unix.ADDR_UNIX display) in
    Lwt.return (of_socket socket)
  | Some i ->
    match int_of_string_opt i with
    | None -> Fmt.failwith "Bad WAYLAND_SOCKET: %S is not an integer!" i
    | Some i ->
      (* The OCaml developers recommend using Obj.magic here: https://github.com/ocaml/ocaml/issues/6948 *)
      assert (Sys.os_type = "Unix");
      Log.info (fun f -> f "Connecting to file descriptor %d" i);
      let socket : Unix.file_descr = Obj.magic i in
      Unix.set_close_on_exec socket;
      Unix.putenv "WAYLAND_SOCKET" "";
      Lwt.return (of_socket (Lwt_unix.of_unix_file_descr socket))
