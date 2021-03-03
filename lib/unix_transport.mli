val of_socket : Lwt_unix.file_descr -> S.transport

val socket_path : ?wayland_display:string -> unit -> string
(** [socket_path ()] returns the path to the wayland socket to use.
    {ol
    {- If $WAYLAND_DISPLAY is set then it returns that.
       If the path is relative then "$XDG_RUNTIME_DIR/$WAYLAND_DISPLAY" is used.}
    {- If $WAYLAND_DISPLAY is not set, it defaults to "wayland-0".}
    }
    @param wayland_display Use this as the value of $WAYLAND_DISPLAY
                           instead of reading it from the environment. *)

val connect : unit -> S.transport Lwt.t
(** [connect ()] connects to the Wayland server's socket:
    {ol
    {- If $WAYLAND_SOCKET is set then that file descriptor is used.}
    {- Otherwise, [socket_path ()] is used. }
    } *)
