type t = {
  buffer : Cstruct.buffer;
  mutable cons : int;        (* Index of next byte to read *)
  mutable prod : int;        (* Index of next byte to write *)
}

let capacity t = Bigarray.Array1.dim t.buffer

let data_avail t =
  t.prod - t.cons

let free_space t =
  capacity t - t.prod

let create size =
  {
    buffer = Bigarray.(Array1.create char c_layout) size;
    cons = 0;
    prod = 0;
  }

let io_vec t =
  if free_space t = 0 && t.cons > 0 then (
    (* We've reached the end of the buffer part-way through a message.
       Move the part we've got to the beginning of the buffer. *)
    let buf = Cstruct.of_bigarray t.buffer in
    let len = data_avail t in
    Cstruct.blit buf t.cons buf 0 len;
    t.cons <- 0;
    t.prod <- len;
  );
  let v = Lwt_unix.IO_vectors.create () in
  Lwt_unix.IO_vectors.append_bigarray v t.buffer t.prod (free_space t);
  v

let update_producer t n =
  assert (n > 0);
  t.prod <- t.prod + n

let update_consumer t n =
  let cons = t.cons + n in
  if cons = t.prod then (
    (* Optimisation: reset the buffer if the consumer catches up with the producer. *)
    t.cons <- 0; t.prod <- 0
  ) else (
    t.cons <- cons
  )

let data t =
  Cstruct.of_bigarray t.buffer ~off:t.cons ~len:(data_avail t)

let dump f t =
  let { buffer = _; prod; cons } = t in
  Fmt.pf f "{ prod = %d; cons = %d }"
    prod
    cons
