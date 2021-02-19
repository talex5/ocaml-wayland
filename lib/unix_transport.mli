val of_socket : Lwt_unix.file_descr -> S.transport

val connect : unit -> S.transport Lwt.t
(** [connect ()] connects to the Wayland server's socket:
    {ol
    {- If $WAYLAND_SOCKET is set then that file descriptor is used.}
    {- Otherwise, if $WAYLAND_DISPLAY is set then it connects to that socket.
       If the path is relative then "$XDG_RUNTIME_DIR/$WAYLAND_DISPLAY" is used.}
    {- If $WAYLAND_DISPLAY is not set, it defaults to "wayland-0".}
    } *)
