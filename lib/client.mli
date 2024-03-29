(** Client-side connection to Wayland display. *)

type t

module type TRACE = Proxy.TRACE with type role = [`Client]

val connect : ?trace:(module TRACE) -> sw:Eio.Switch.t -> #S.transport -> t
(** [connect ~sw transport] runs the Wayland protocol over [transport]
    (typically created with {!Unix_transport.connect}).

    It spawns a background thread (attached to [sw]) to handle incoming messages.
    If the thread gets an error then the switch will be cancelled.

    The caller is responsible for ensuring [transport] is closed,
    but the library will shut it down if it gets end-of-file from the server.

    @param trace Used to trace all messages sent and received.
                 The default tracer logs messages at debug level, and the log's source is set to debug level
                 if $WAYLAND_DEBUG is "1" or "client" the first time {!connect} is called. *)

val sync : t -> unit
(** Send a sync message to the server and wait for the reply.
    This ensures that all previous messages have been received. *)

val wl_display : t -> [`V1] Wayland_client.Wl_display.t
(** [wl_display t] returns the initial object for the display.
    You probably want to use {!Registry.of_display} instead. *)

val stop : t -> unit
(** [stop t] shuts down [t]'s transport, causing the server to receive end-of-file. *)

val dump : t Fmt.t
(** Dump the state of the connection for debugging. *)
