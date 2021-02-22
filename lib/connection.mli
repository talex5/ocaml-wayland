type t

val connect :
  S.transport ->
  ('a, 'v) Proxy.Service_handler.t ->
  (t * ('a, 'v) Proxy.t)

val listen : t -> unit Lwt.t
