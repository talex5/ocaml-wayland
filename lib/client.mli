(** Client-side connection to Wayland display. *)

type t

module type TRACE = Proxy.TRACE with type role = [`Client]

val connect : ?trace:(module TRACE) ->
  #S.transport -> t * (unit, exn) Lwt_result.t
(** [connect transport] runs the Wayland protocol over [transport]
    (typically created with {!Unix_transport.connect}).
    It spawns a background thread to handle incoming messages.
    It returns the new connection and a promise that resolves when
    the connection ends.
    The caller is responsible for closing [transport] when done.
    @param trace Used to trace all messages sent and received.
                 The default tracer logs messages at debug level, and the log's source is set to debug level
                 if $WAYLAND_DEBUG is "1" or "client" the first time {!connect} is called. *)

val sync : t -> unit Lwt.t
(** Send a sync message to the server and wait for the reply.
    This ensures that all previous messages have been received. *)

val wl_display : t -> [`V1] Wayland_client.Wl_display.t
(** [wl_display t] returns the initial object for the display.
    You probably want to use {!Registry.of_display} instead. *)

val set_paused : t -> bool -> unit
(** [set_paused t] sets the paused state.
    No further incoming messages will be dispatched while [t] is paused. *)

val dump : t Fmt.t
(** Dump the state of the connection for debugging. *)
