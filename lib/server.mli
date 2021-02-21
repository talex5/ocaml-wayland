type t

val connect : S.transport -> (([`Wl_registry], [`V1]) Proxy.t -> unit) -> t
(** [connect transport registry] runs the Wayland protocol over [transport]
    (typically created with {!Unix_transport.of_socket}).
    It spawns a background thread to handle incoming messages.
    [registry] is used to handle requests for the display's registry.
    You must call {!Proxy.Service_handler.attach registry} before returning
    or using the proxy. *)

val wl_display : t -> [`V1] Wayland_server.Wl_display.t
