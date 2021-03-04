type 'a ty = ..

type param = [
  | `Uint 
  | `Int 
  | `String 
  | `Array 
  | `Object of string option
  | `New_ID of string option
  | `Fixed 
  | `FD 
]

type arg = string * param
(** Argument name and type. *)

type info = int -> string * arg list
(** A function that takes an operation number and returns the operation name and argument metadata. *)

module type S = sig
  type t
  type _ ty += T : t ty

  val interface : string
  val version : int32
  val requests : info
  val events : info
end
