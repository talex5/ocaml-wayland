(** Client-side connection to Wayland display. *)

type t

val connect : S.transport -> t
(** [connect transport] runs the Wayland protocol over [transport]
    (typically created with {!Unix_transport.connect}).
    It spawns a background thread to handle incoming messages. *)

val sync : t -> unit Lwt.t
(** Send a sync message to the server and wait for the reply.
    This ensures that all previous messages have been received. *)

val wl_display : t -> [`V1] Wayland_client.Wl_display.t
(** [wl_display t] returns the initial object for the display.
    You probably want to use {!Registry.of_display} instead. *)
