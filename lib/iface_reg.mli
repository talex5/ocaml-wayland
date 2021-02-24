val register : (module Metadata.S) -> unit
(** Called by the generated code to register each Wayland interface. *)

val lookup : string -> (module Metadata.S)
(** Called by the generated code to convert a string interface to a variant. *)
