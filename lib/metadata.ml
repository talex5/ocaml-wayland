type ty = [
  | `Uint 
  | `Int 
  | `String 
  | `Array 
  | `Object of string option
  | `New_ID of string option
  | `Fixed 
  | `FD 
]

type arg = string * ty
(** Argument name and type. *)

type info = int -> string * arg list
(** A function that takes an operation number and returns the operation name and argument metadata. *)

module type S = sig
  val interface : string
  val requests : info
  val events : info
end
