type 'a t

val connect :
  sw:Eio.Switch.t ->
  trace:(module Proxy.TRACE with type role = 'role) ->
  ([< `Client | `Server] as 'role) ->
  #S.transport ->
  ([`Wl_display], 'v, 'role) #Proxy.Service_handler.t ->
  ('role t * ([`Wl_display], 'v, 'role) Proxy.t)

val stop : [< `Client | `Server ] t -> unit

val dump : _ t Fmt.t
