type t

val connect : unit -> t Lwt.t
(** [connect ()] connects to the Wayland server and spawns a thread to handling incoming messages:
    {ol
    {- If $WAYLAND_SOCKET is set then that file descriptor is used.}
    {- Otherwise, if $WAYLAND_DISPLAY is set then it connects to that socket.
       If the path is relative then "$XDG_RUNTIME_DIR/$WAYLAND_DISPLAY" is used.}
    {- If $WAYLAND_DISPLAY is not set, it defaults to "wayland-0".}
    } *)

val sync : t -> unit Lwt.t
(** Send a sync message to the server and wait for the reply.
    This ensures that all previous messages have been received. *)

val wl_display : t -> [`V1] Wayland_client.Wl_display.t
(** [wl_display t] returns the initial object for the display.
    You probably want to use {!Registry.of_display} instead. *)
