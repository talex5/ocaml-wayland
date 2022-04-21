type t = <
  S.transport;
  close : unit;
  socket : Eio_unix.Net.stream_socket_ty Eio.Resource.t;
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

let max_fds = 32       (* libwayland uses 28. Must be more than that so we can detect if it fails. *)

let of_socket socket : t =
  object (_ : #S.transport)
    val mutable up = true

    method send data fds =
      Eio_unix.Net.send_msg socket ~fds [data]

    method recv ~sw buf =
      try
        let n, fds = Eio_unix.Net.recv_msg_with_fds ~sw ~max_fds socket [buf] in
        assert (List.length fds < max_fds);     (* Otherwise, some may have been lost. *)
        n, fds
      with
      | End_of_file
      | Eio.Io (Eio.Net.E Connection_reset _, _) ->
        up <- false;
        (0, [])

  method shutdown =
    up <- false;
    Eio.Flow.shutdown socket `Send

  method up = up

  method socket = (socket :> Eio_unix.Net.stream_socket_ty Eio.Resource.t)

  method close =
    up <- false;
    Eio.Flow.close socket

  method pp f = Eio_unix.Fd.pp f (Eio_unix.Net.fd socket)
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

let connect ~sw ~net () =
  match Sys.getenv_opt "WAYLAND_SOCKET" with
  | None | Some "" ->
    let display = socket_path () in
    Log.info (fun f -> f "Connecting to %S" display);
    let socket = Eio.Net.connect ~sw net (`Unix display) in
    of_socket socket
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
      let socket = Eio_unix.Net.import_socket_stream ~sw ~close_unix:true socket in
      of_socket socket
