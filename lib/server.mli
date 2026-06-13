type t

module type TRACE = Proxy.TRACE with type role = [`Server]

val connect : ?trace:(module TRACE) -> sw:Eio.Switch.t ->
  #S.transport -> ([`Wl_display], [`V1], [`Server]) #Proxy.Service_handler.t -> t
(** [connect transport handler] runs the Wayland protocol over [transport]
    (typically created with {!Unix_transport.of_socket}).

    It spawns a background thread (attached to [sw]) to handle incoming messages.
    If the thread gets an error then the switch will be cancelled.

    The caller is responsible for ensuring [transport] is closed,
    but the library will shut it down if it gets end-of-file from the client.

    @param trace Used to trace all messages sent and received.
                 The default tracer logs messages at debug level, and the log's source is set to debug level
                 if $WAYLAND_DEBUG is "1" or "server" the first time {!connect} is called. *)

val wl_display : t -> [`V1] Wayland_server.Wl_display.t

val stop : t -> unit
(** [stop t] shuts down [t]'s transport, causing the client to receive end-of-file. *)

val dump : t Fmt.t
(** Dump the state of the connection for debugging. *)

val implementation_error : t -> string -> unit
(** Disconnect the client with a message indicating a compositor bug. *)

val no_memory : t -> string -> unit
(** Disconnect the client with a message indicating that the server ran out of memory. *)
