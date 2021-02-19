type t

val connect :
  S.transport ->
  ('a, 'v, _, 'v) Proxy.handler ->
  (t * ('a, 'v) Proxy.t)

val listen : t -> unit Lwt.t
