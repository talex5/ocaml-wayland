type t = int32

let of_int i = Int32.shift_left (Int32.of_int i) 8
let to_int t = Int32.to_int (Int32.shift_right t 8)

let of_bits = Fun.id
let to_bits = Fun.id

let pp f t = Fmt.pf f "%lx" t
