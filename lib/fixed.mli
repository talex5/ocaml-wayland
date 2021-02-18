(** A 24.8 fixed-point number. *)

type t = private int32

val of_int : int -> t
val to_int : t -> int

val of_bits : int32 -> t
val to_bits : t -> int32

val pp : t Fmt.t
