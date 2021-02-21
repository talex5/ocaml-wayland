type 'a ty = ..

type interface = Interface : 'a ty * (module Metadata.S) -> interface

val register : 'a ty -> (module Metadata.S) -> unit
(** Called by the generated code to register each Wayland interface. *)

val lookup : string -> interface
(** Called by the generated code to convert a string interface to a variant. *)

(** Returned by [lookup] if the interface name isn't registered. *)
module Unknown : sig
  include Metadata.S
  type 'a ty += T : string -> [`Unknown] ty
end
