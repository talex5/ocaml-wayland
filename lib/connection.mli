type 'a t

val connect :
  ([< `Client | `Server] as 'role) ->
  S.transport ->
  ('a, 'v, 'role) Proxy.Service_handler.t ->
  ('role t * ('a, 'v, 'role) Proxy.t)

val listen : [< `Client | `Server ] t -> unit Lwt.t
