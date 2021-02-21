type 'a ty = ..

type interface = Interface : 'a ty * (module Metadata.S) -> interface

module Unknown = struct
  let interface = "unknown"

  let events _ = "unknown", []
  let requests _ = "unknown", []

  type 'a ty += T : string -> [`Unknown] ty
end

let interfaces = Hashtbl.create 100

let register ty (module M : Metadata.S) =
  if Hashtbl.mem interfaces M.interface then
    Fmt.failwith "Wayland interface type %S is already registered!" M.interface;
  Hashtbl.add interfaces M.interface (Interface (ty, (module M)))

let lookup interface =
  match Hashtbl.find_opt interfaces interface with
  | Some x -> x
  | None -> Interface (Unknown.T interface, (module Unknown))
