type t

module type TRACE = Proxy.TRACE with type role = [`Server]

val connect : ?trace:(module TRACE) ->
  S.transport -> ([`Wl_display], [`V1], [`Server]) #Proxy.Service_handler.t -> t
(** [connect transport handler] runs the Wayland protocol over [transport]
    (typically created with {!Unix_transport.of_socket}).
    It spawns a background thread to handle incoming messages. *)

val wl_display : t -> [`V1] Wayland_server.Wl_display.t

val closed : t -> (unit, exn) Lwt_result.t
(** [closed t] resolves when the connection is closed (either due to the other side closing it normally
    or because an exception was raised. *)
