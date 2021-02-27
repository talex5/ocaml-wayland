module type ENDIAN = (module type of Cstruct.BE)

let ne =
  if Sys.big_endian then (module Cstruct.BE : ENDIAN)
  else (module Cstruct.LE : ENDIAN)

(* Native endian *)
module NE = (val ne)

type 'rw generic = {
  buffer : Cstruct.t;
  fds : Unix.file_descr Queue.t;
  mutable next : int;           (* The index of the next argument to read or write. *)
} constraint 'rw = [< `R | `W]

type ('a, 'rw) t = 'rw generic

let obj t =
  NE.get_uint32 t.buffer 0

let op t =
  if Sys.big_endian then (
    NE.get_uint16 t.buffer 6
  ) else (
    NE.get_uint16 t.buffer 4
  )

let get_int t =
  let x = NE.get_uint32 t.buffer t.next in
  t.next <- t.next + 4;
  x

let add_int t x =
  NE.set_uint32 t.buffer t.next x;
  t.next <- t.next + 4

let get_string t =
  let cs = Cstruct.shift t.buffer t.next in
  let len_excl_term = (NE.get_uint32 cs 0 |> Int32.to_int) - 1 in
  t.next <- t.next + 4 + ((len_excl_term + 4) land -4);
  Cstruct.to_string cs ~off:4 ~len:len_excl_term

let get_string_opt t =
  let cs = Cstruct.shift t.buffer t.next in
  let len_excl_term = (NE.get_uint32 cs 0 |> Int32.to_int) - 1 in
  t.next <- t.next + 4 + ((len_excl_term + 4) land -4);
  if len_excl_term = -1 then None
  else Some (Cstruct.to_string cs ~off:4 ~len:len_excl_term)

let add_string t v =
  let len_excl_term = String.length v in
  add_int t (Int32.of_int (len_excl_term + 1));
  Cstruct.blit_from_string v 0 t.buffer t.next len_excl_term;
  t.next <- t.next + ((len_excl_term + 4) land -4)

let add_string_opt t = function
  | None -> add_int t Int32.zero
  | Some v -> add_string t v

let get_array t =
  let cs = Cstruct.shift t.buffer t.next in
  let len = NE.get_uint32 cs 0 |> Int32.to_int in
  t.next <- t.next + 4 + ((len + 3) land -4);
  Cstruct.to_string cs ~off:4 ~len

let add_array t v =
  let len = String.length v in
  add_int t (Int32.of_int len);
  Cstruct.blit_from_string v 0 t.buffer t.next len;
  t.next <- t.next + ((len + 3) land -4)

let get_fd t =
  Queue.pop t.fds

let add_fd t v =
  Queue.add (Unix.dup v) t.fds

let get_fixed t =
  get_int t |> Fixed.of_bits

let add_fixed t v =
  add_int t (Fixed.to_bits v)

let rec count_strings acc = function
  | [] -> acc
  | None :: ss ->
    count_strings (acc + 4) ss
  | Some s :: ss ->
    let len = 4 + (String.length s + 4) land -4 in (* Note: includes ['\0'] terminator *)
    count_strings (acc + len) ss

let rec count_arrays acc = function
  | [] -> acc
  | x :: xs ->
    let len = 4 + (String.length x + 3) land -4 in
    count_arrays (acc + len) xs

let alloc ~obj ~op ~ints ~strings ~arrays =
  let len = count_arrays (count_strings (8 + ints * 4) strings) arrays in
  let buffer = Cstruct.create len in
  NE.set_uint32 buffer 0 obj;
  if Sys.big_endian then (
    NE.set_uint16 buffer 4 len;
    NE.set_uint16 buffer 6 op;
  ) else (
    NE.set_uint16 buffer 6 len;
    NE.set_uint16 buffer 4 op;
  );
  { buffer; next = 8; fds = Queue.create () }

let buffer t = t.buffer.buffer

let parse ~fds cs =
  if Cstruct.length cs >= 8 then (
    let len =
      if Sys.big_endian then (
        NE.get_uint16 cs 4
      ) else (
        NE.get_uint16 cs 6
      )
    in
    if Cstruct.len cs >= len then (
      Some { buffer = Cstruct.sub cs 0 len; next = 8; fds }
    ) else (
      None
    )
  ) else (
    None
  )

let length t = Cstruct.length t.buffer

let fds t = t.fds

let cast = Fun.id

let pop_and_show_arg f t : Metadata.param -> unit = function
  | `Int ->
    Fmt.pf f "%ld" (get_int t)
  | `Uint ->
    Fmt.pf f "%lu" (get_int t)
  | `Fixed ->
    Fixed.pp f (get_fixed t)
  | `Object _ ->
    begin match get_int t with
      | 0l -> Fmt.string f "null"
      | i -> Fmt.pf f "%lx" i
    end
  | `New_ID None ->
    let interface = get_string t in
    let version = get_int t in
    let id = get_int t in
    Fmt.pf f "(%s_%ld)%lx" interface version id
  | `New_ID (Some _) ->
    Fmt.pf f "+%lx" (get_int t)
  | `String ->
    Fmt.(option ~none:(unit "null") (quote string)) f (get_string_opt t)
  | `Array ->
    Fmt.(quote string) f (get_array t)
  | `FD ->
    Fmt.string f "(fd)"

let pp_args types f t =
  let t = { t with next = 8 } in
  let rec loop = function
    | [] -> ()
    | (name, ty) :: tys ->
      Fmt.pf f "%s:" name;
      pop_and_show_arg f t ty;
      if tys <> [] then Fmt.sp f ();
      loop tys
  in
  loop types
