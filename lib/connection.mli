type 'a t

val connect :
  sw:Eio.Switch.t ->
  trace:(module Proxy.TRACE with type role = 'role) ->
  ([< `Client | `Server] as 'role) ->
  #S.transport ->
  ('a, 'v, 'role) #Proxy.Service_handler.t ->
  ('role t * ('a, 'v, 'role) Proxy.t)

val stop : [< `Client | `Server ] t -> unit

val dump : _ t Fmt.t
