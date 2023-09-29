(** A simple client-side wrapper for the Wayland registry. *)

type entry = {
  name : int32;
  version : int32;
}

type t

type ('a, 'v) handler = <
  ('a, 'v, [`Client]) Proxy.Service_handler.t;
  bind_version : 'v;
>

val of_display : Client.t -> t
(** [of_display d] creates a new registry from a display.
    It performs a sync before returning the result, so that the registry is fully-populated. *)

val get : t -> string -> entry list
(** [get interface] returns the entries for interface type [interface]. *)

val get_exn : t -> string -> entry
(** [get_exn interface] returns the first entry for [interface], or raises an exception if its not present. *)

val bind : t -> <('a, 'v) handler; ..> -> ('a, 'v, [`Client]) Proxy.t
(** [bind t handler] gets the entry for [handler]'s interface,
    checks that the version is compatible, and creates a proxy for it.
    Raises an exception if the interface isn't listed, or has the wrong version. *)

val wl_registry : t -> [`V1] Wayland_client.Wl_registry.t
