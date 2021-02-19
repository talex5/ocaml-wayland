type t

val create : int -> t
(** [create size] is a fresh buffer that can hold up to [size] bytes. *)

val free_buffer : t -> Cstruct.t
(** [free_buffer t] is the contiguous free space in the buffer, for the next read operation. *)

val update_producer : t -> int -> unit
(** [update_producer t got] records that [got] bytes were written to [free_buffer t]. *)

val data_avail : t -> int
(** [data_avail t] is the number of bytes of data available in the buffer. *)

val data : t -> Cstruct.t
(** [data t] returns a view onto the data in the buffer.
    The cstruct is valid until the next call of [update_consumer] or [io_vec]. *)

val update_consumer : t -> int -> unit
(** [update_consumer t len] records that [len] bytes have been consumed. *)

val dump : t Fmt.t
