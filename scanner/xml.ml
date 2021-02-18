type t = {
  pos : string * Xmlm.pos;
  name : Xmlm.name;
  mutable attrs : Xmlm.attribute list;
  mutable children : tree list;
}
and tree = E of t | D of string

let pp f node =
  let file, (line, col) = node.pos in
  Fmt.pf f "<%a> at %s:%d:%d" Xmlm.pp_name node.name file line col

let check_empty node =
  node.attrs |> List.iter (function
      | (("", name), _) -> Fmt.failwith "Unexpected attribute %S in %a" name pp node
      | _ -> ()
    );
  node.children |> List.iter (function
      | E ({ name = ("", _); _ } as n) -> Fmt.failwith "Unexpected element %a" pp n
      | D d when String.trim d <> "" -> Fmt.failwith "Unexpected text %S in %a" d pp node
      | _ -> ()
    )

let take_elements name node f =
  let name = ("", name) in
  let rec aux = function
    | [] -> [], []
    | E e :: xs when e.name = name ->
      let es, xs = aux xs in
      (e :: es), xs
    | n :: xs ->
      let es, xs = aux xs in
      es, (n :: xs)
  in
  let chosen, rest = aux node.children in
  node.children <- rest;
  chosen |> List.map @@ fun c ->
  let r = f c in
  check_empty c;
  r

let take_sole_opt name node f =
  match take_elements name node f with
  | [] -> None
  | [ x ] -> Some x
  | _ -> Fmt.failwith "Multiple %S children in %a!" name pp node

let take_sole name node f =
  match take_sole_opt name node f with
  | Some x -> x
  | None -> Fmt.failwith "No %S child in %a!" name pp node

let take_data node =
  let rec aux = function
    | [] -> [], []
    | D d :: xs ->
      let ds, xs = aux xs in
      (d :: ds), xs
    | n :: xs ->
      let ds, xs = aux xs in
      ds, (n :: xs)
  in
  let chosen, rest = aux node.children in
  node.children <- rest;
  match chosen with
  | [] -> ""
  | [d] -> d
  | _ -> Fmt.failwith "Element %a has multiple data nodes (mixed content)" pp node


let take_attr_opt name n =
  let (let+) x f = Option.map f x in
  let rec extract = function
    | [] -> None
    | (("", k), v) :: xs when k = name -> Some (v, xs)
    | x :: xs ->
      let+ r, xs = extract xs in
      r, (x :: xs)
  in
  let+ r, xs = extract n.attrs in
  n.attrs <- xs;
  r

let take_attr name node =
  match take_attr_opt name node with
  | Some x -> x
  | None -> Fmt.failwith "Missing attribute %S on %a" name pp node

let parse ~name ch f =
  let input = Xmlm.make_input (`Channel ch) in
  let rec aux ~level =
    let pos = name, Xmlm.pos input in
    match Xmlm.input input with
    | `El_start (name, attrs) ->
      let children = aux ~level:(succ level) in
      let rest = if level = 0 then [] else aux ~level in
      E {pos; name; attrs; children} :: rest
    | `Data d -> D d :: aux ~level
    | `Dtd _ -> aux ~level
    | `El_end -> []
  in
  match aux ~level:0 with
  | [ E e ] -> f e
  | _ -> Fmt.failwith "Malformed XML in %s" name
