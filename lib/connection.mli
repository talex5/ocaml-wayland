type t

val connect :
  ('a, 'v, _, 'v) Proxy.handler ->
  (t * ('a, 'v) Proxy.t) Lwt.t

val listen : t -> unit Lwt.t
