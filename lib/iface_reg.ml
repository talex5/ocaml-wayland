let interfaces = Hashtbl.create 100

let register (module M : Metadata.S) =
  if Hashtbl.mem interfaces M.interface then
    Fmt.failwith "Wayland interface type %S is already registered!" M.interface;
  Hashtbl.add interfaces M.interface (module M : Metadata.S)

let lookup interface =
  match Hashtbl.find_opt interfaces interface with
  | Some x -> x
  | None -> Fmt.failwith "Unknown Wayland interface %S" interface
