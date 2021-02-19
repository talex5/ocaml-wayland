open Schema

let module_name = String.capitalize_ascii

let full_module_name (proto : Protocol.t) (iface : Interface.t) =
  Fmt.strf "%s_proto.%s" (String.capitalize_ascii proto.name) (module_name iface.name)

let mangle name =
  let name =
    name |> String.map @@ function
    | '-' -> '_'
    | x -> x
  in
  match name with
  | "method"
  | "handlers"
  | "h"
  | "done" -> name ^ "_"
  | x -> x

let pp_poly f name =
  Fmt.pf f "[`%s]" (String.capitalize_ascii (mangle name))

let pp_type f (ty:Arg.ty) =
  Fmt.string f @@
  match ty with
  | `Uint -> "int32"
  | `Int -> "int32"
  | `String -> "string"
  | `Array -> "string"
  | `Object _ -> "int32"
  | `New_ID _ -> "int32"
  | `Fixed -> "Fixed.t"
  | `FD -> "Unix.file_descr"

let pp_type_getter f (ty:Arg.ty) =
  Fmt.string f @@
  match ty with
  | `Uint -> "int"
  | `Int -> "int"
  | `String -> "string"
  | `Array -> "array"
  | `Object _ -> "int"
  | `New_ID _ -> "int"
  | `Fixed -> "fixed"
  | `FD -> "fd"

let named_argument (arg : Arg.t) =
  match arg.ty with
  | `New_ID _ -> false
  | `Object _ -> arg.name <> "id"
  | _ -> true

let pp_arg f arg =
  if named_argument arg then
    Fmt.pf f "%s:%a" (mangle arg.name) pp_type arg.ty
  else
    pp_type f arg.ty

let pp_sig f = function
  | [] -> Fmt.string f "unit"
  | args -> Fmt.(list ~sep:(unit " ->@ ") pp_arg ++ any " ->@ unit") f args

let pp_args ~with_types =
  let pp_arg f arg =
    if named_argument arg then Fmt.string f "~";
    let m = mangle arg.name in
    match arg.ty with
    | `Object (Some interface) when with_types ->
      Fmt.pf f "(%s:(%a, _) Proxy.t)" m pp_poly interface
    | `New_ID (Some interface) when with_types ->
      Fmt.pf f "(%s:(%a, 'v, _, _) Proxy.handler)" m pp_poly interface
    | _ ->
      Fmt.pf f "%s" m
  in
  Fmt.(list ~sep:sp) pp_arg

let variant_of_ty (arg : Arg.t) =
  match arg.ty with
  | `Uint -> "`Uint"
  | `Int -> "`Int"
  | `String -> "`String"
  | `Array -> "`Array"
  | `Object interface -> Fmt.strf "`Object (%a)" Fmt.Dump.(option string) interface
  | `New_ID interface -> Fmt.strf "`New_ID (%a)" Fmt.Dump.(option string) interface
  | `Fixed -> "`Fixed"
  | `FD -> "`FD"

let pp_arg_info =
  let pp_arg f (arg : Arg.t) =
    Fmt.pf f "%S, %s" arg.name (variant_of_ty arg)
  in
  Fmt.(list ~sep:semi) pp_arg

let pp_versions f (min, max) =
  Fmt.(list ~sep:(unit " | ") (fmt "`V%d"))
    f (List.init (max + 1 - min) (fun x -> min + x))

let op_info name f messages =
  let pp_ops f =
    List.iteri (fun i (msg : Message.t) ->
        Fmt.pf f "| %d -> %S, [@[%a@]]@," i msg.name pp_arg_info msg.args
      )
  in
  Fmt.pf f "function@,\
    %a\
    | i -> Proxy.unknown_%s i, []"
    pp_ops messages
    name

let with_output path pp =
  let ch = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out ch)
    (fun () ->
       let f = Format.formatter_of_out_channel ch in
       Format.pp_set_margin f 120;
       Fmt.pf f "@[<v>(* This file was generated automatically by wayland-scanner-ocaml *)@,";
       pp f;
       Format.pp_print_flush f ()
    )

let trim_lines xs =
  let rec aux1 = function
    | "" :: xs -> aux1 xs
    | xs -> aux2 xs
  and aux2 = function
    | [] -> []
    | [""] -> []
    | x :: xs -> x :: aux2 xs
  in
  aux1 xs

let comment f = function
  | None -> ()
  | Some (d : Description.t) ->
    let full = String.split_on_char '\n' d.full |> List.map String.trim |> trim_lines in
    Fmt.pf f "@,(** @[<v>%s.@,@,%a@] *)" (String.capitalize_ascii d.summary) Fmt.(list ~sep:cut string) full

let pp_strings f args =
  args
  |> List.filter_map (fun (a : Arg.t) ->
      match a.ty with
      | `New_ID None -> Some (Fmt.strf "(Proxy.handler_interface %s)" (mangle a.name))
      | `String -> Some (mangle a.name)
      | _ -> None
    )
  |> Fmt.(list ~sep:semi string) f

let rec root_version ~parents (interface : Interface.t) =
  match Parent.parent parents interface with
  | None -> interface.version
  | Some parent ->
    if parent.version < interface.version then (
      Fmt.failwith "Interface %S has version %d, which is less that its child %S at %d!"
        parent.name parent.version
        interface.name interface.version
    );
    root_version ~parents parent

type version_group = {
  versions : int list;  (* List of identical versions *)
  requests : (int * Message.t) list;
  events : (int * Message.t) list;
}

(* Sort the messages of [iface] into groups by version, from 1 to [n_versions]. *)
let get_versions ~n_versions (iface : Interface.t) =
  let groups = Array.init n_versions (fun v -> { requests = []; events = []; versions = [v + 1] }) in
  (* Sort messages into groups by version *)
  iface.requests |> List.iteri (fun i (m : Message.t) ->
      let prev = groups.(m.since - 1) in
      groups.(m.since - 1) <- { prev with requests = (i, m) :: prev.requests }
    );
  iface.events |> List.iteri (fun i (m : Message.t) ->
      let prev = groups.(m.since - 1) in
      groups.(m.since - 1) <- { prev with events = (i, m) :: prev.events }
    );
  (* Combine each empty version with its previous version. *)
  for i = n_versions downto 2 do
    match groups.(i - 1) with
    | { requests = []; events = []; versions } ->
      let prev = groups.(i - 2) in
      groups.(i - 2) <- { prev with versions = prev.versions @ versions }
    | _ -> ()
  done;
  groups |> Array.to_list |> List.filter (function
      | { requests = []; events = []; versions = _ } -> false
      | _ -> true
    )

let pp_ctor f enum =
  let enum =
    match enum.[0] with
    | '0' .. '9' -> "V" ^ enum
    | _ -> enum
  in
  Fmt.string f (String.capitalize_ascii enum)

let pp_enum f (enum : Enum.t) =
  Fmt.pf f "@,";
  comment f enum.description;
  Fmt.pf f "@,@[<v2>module %s = struct" (module_name enum.name);
  if enum.bitfield then (
    Fmt.pf f "@,type t = int32";
    enum.entries |> List.iter (fun (e : Entry.t) ->
        Fmt.pf f "@,";
        comment f e.description;
        Fmt.pf f "@,let %s = %ld" (mangle e.name) e.value
      );
    Fmt.pf f "@,";
    Fmt.pf f "@,let to_int32 = Fun.id";
    Fmt.pf f "@,let of_int32 = Fun.id";
  ) else (
    Fmt.pf f "@,@[<v2>type t =";
    enum.entries |> List.iter (fun (e : Entry.t) ->
        Fmt.pf f "@,@[<v2>| %a : t%a@]" pp_ctor e.name
          comment e.description
      );
    Fmt.pf f "@]@,@,@[<v2>let to_int32 = function";
    enum.entries |> List.iter (fun (e : Entry.t) ->
        Fmt.pf f "@,| %a -> %ldl" pp_ctor e.name e.value
      );
    Fmt.pf f "@]@,@,@[<v2>let of_int32 = function";
    enum.entries |> List.iter (fun (e : Entry.t) ->
        Fmt.pf f "@,| %ldl -> %a" e.value pp_ctor e.name
      );
    Fmt.pf f {|@,| x -> Fmt.failwith "Invalid %s enum value %%ld" x|} enum.name;
    Fmt.pf f "@]"
  );
  Fmt.pf f "@]@,end"

let pp_enum_link (protocol : Protocol.t) (iface : Interface.t) f (enum : Enum.t) =
  Fmt.pf f "@,module %s = %s_proto.%s.%s"
    (module_name enum.name)
    (module_name protocol.name)
    (module_name iface.name)
    (module_name enum.name)

let make_wrappers ~internal role (protocol : Protocol.t) f =
  let parents = Parent.index protocol in
  let line fmt = Fmt.pf f ("@," ^^ fmt) in
  let _incoming, _outgoing =
    match role with
    | `Client -> "event", "request"
    | `Server -> "request", "event"
  in
  line {|[@@@@@@ocaml.warning "-27"]|};
  if not internal then (
    line "@,module Proxy = Wayland.Proxy";
    line "module Msg = Wayland.Msg";
    line "module Fixed = Wayland.Fixed";
  );
  line "";
  protocol.interfaces |> List.iter (fun (iface : Interface.t) ->
      let n_versions = root_version ~parents iface in
      let versions = get_versions ~n_versions iface in
      line "";
      comment f iface.description;
      line "@[<v2>module %s = struct" (module_name iface.name);
      line "type 'v t = (%a, 'v) Proxy.t" pp_poly iface.name;
      Fmt.list (pp_enum_link protocol iface) f iface.enums;
      let prev_version = ref 0 in
      let have_incoming = ref false in
      versions |> List.iter (fun (group : version_group) ->
          line "";
          line "(** {2 Version @[<h>%a@]} *)" Fmt.(list ~sep:comma int) group.versions;
          let version = List.hd group.versions in
          let msgs_in, msgs_out =
            match role with
            | `Client -> group.events, group.requests
            | `Server -> group.requests, group.events
          in
          if msgs_in <> [] then have_incoming := true;
          (* Sending messages *)
          msgs_out |> List.iter (fun (i, (msg : Message.t)) ->
              let new_ids = List.filter (function { Arg.ty = `New_ID _; _} -> true | _ -> false) msg.args in
              let extra_version_fields =
                new_ids |> List.filter (fun (a : Arg.t) -> a.ty = `New_ID None) |> List.length
              in
              line "";
              comment f msg.description;
              line "@[<v2>let %s (_t:([< %a] as 'v) t) @[<h>%a@] =" (mangle msg.name) pp_versions (msg.since, n_versions) (pp_args ~with_types:true) msg.args;
              new_ids |> List.iter (fun (arg : Arg.t) ->
                  let m = mangle arg.name in
                  line "let __%s = Proxy.spawn%s _t %s in"
                    m
                    (match arg.ty with `New_ID None -> "_bind" | _ -> "")
                    m
                );
              line "let _msg = Proxy.alloc _t ~op:%d ~ints:%d ~strings:[%a] in"
                i (List.length msg.args + extra_version_fields) pp_strings msg.args;
              msg.args |> List.iter (fun (arg : Arg.t) ->
                  let m = mangle arg.name in
                  match arg.ty with
                  | `New_ID None ->
                    line "Msg.add_string _msg (Proxy.handler_interface %s);" m;
                    line "Msg.add_int _msg (Proxy.handler_version %s);" m;
                    line "Msg.add_int _msg (Proxy.id __%s);" m
                  | `New_ID (Some _) ->
                    line "Msg.add_int _msg (Proxy.id __%s);" m
                  | `Object _ ->
                    line "Msg.add_int _msg (Proxy.id %s);" m
                  | `Int | `Uint when arg.enum <> None ->
                    let enum =
                      Option.get arg.enum
                      |> String.split_on_char '.'
                      |> List.map module_name
                    in
                    let enum =
                      match enum with
                      | [leaf] -> [module_name iface.name; leaf]
                      | x -> x
                    in
                    line "Msg.add_int _msg (%s_proto.%a.to_int32 %s);"
                      (module_name protocol.name)
                      Fmt.(list ~sep:(unit ".") string) enum m
                  | _ ->
                    line "Msg.add_%a _msg %s;" pp_type_getter arg.ty m
                );
              line "Proxy.send _t _msg";
              if msg.ty = `Destructor then
                Fmt.pf f ";@,Proxy.invalidate _t";
              if new_ids = [] then Fmt.pf f "@]"
              else (
                Fmt.pf f ";";
                let arg_name f arg = Fmt.pf f "__%s" (mangle arg.Arg.name) in
                line "%a@]"
                  Fmt.(list ~sep:comma arg_name) new_ids
              )
            );
          (* The type of handlers for incoming messages *)
          line "";
          line "@[<v2>class virtual ['v] h%d = object" version;
          if version > 1 then line "inherit ['v] h%d" !prev_version;
          line "";
          msgs_in |> List.iter (fun (_, (msg : Message.t)) ->
              line "method virtual on_%s : 'v t -> @[%a@]" msg.name pp_sig msg.args;
              comment f msg.description;
              Fmt.cut f ()
            );
          Fmt.pf f "@]@,end";
          line "";
          line "(**/**)";
          line "@[<v2>let _handle_v%d (_handlers:_ #h%d) _proxy _msg =" version version;
          line "match Msg.op _msg with";
          msgs_in |> List.iter (fun (i, (msg : Message.t)) ->
              line "@[<v2>| %d ->" i;
              msg.args |> List.iter (fun (arg : Arg.t) ->
                  line "let %s = Msg.get_%a _msg in" (mangle arg.name) pp_type_getter arg.ty;
                );
              line "_handlers#on_%s _proxy @[%a@]@]" msg.name (pp_args ~with_types:false) msg.args;
            );
          if version > 1 then
            line "| _ -> _handle_v%d _handlers _proxy _msg@]" !prev_version
          else
            line "| _ -> assert false@]";
          line "(**/**)";
          line "";
          group.versions |> List.iter (fun minor_version ->
              if !have_incoming then (
                line "@[<v2>let v%d (handlers:'v h%d)" minor_version version;
              ) else (
                line "@[<v2>let v%d ()" minor_version;
              );
              Fmt.pf f " : (_, 'v, [< %a], [> `V%d]) Proxy.handler = Proxy.handler" pp_versions (1, minor_version) minor_version;
              line "(module %s)" (full_module_name protocol iface);
              line "~version:%dl" minor_version;
              if !have_incoming then (
                line "(_handle_v%d handlers)" version
              ) else (
                line "(_handle_v%d (object end))" version
              );
              Fmt.pf f "@]";
            );
          prev_version := version
        );
      Fmt.pf f "@]@,end"; (* Interface *)
    )

let output ~internal (protocol : Protocol.t) =
  let file_base = mangle protocol.name in
  with_output (file_base ^ "_proto.ml") (fun f ->
      let line fmt = Fmt.pf f ("@," ^^ fmt) in
      if not internal then (
        line "module Proxy = Wayland.Proxy"
      );
      protocol.interfaces |> List.iter (fun (iface : Interface.t) ->
          line "@[<v2>module %s = struct" (module_name iface.name);
          line "let interface = %S" iface.name;
          Fmt.(list ~sep:cut) pp_enum f iface.enums;
          line "";
          line "@[<v2>let requests = %a@]@," (op_info "request") iface.requests;
          line "@[<v2>let events = %a@]@]" (op_info "event") iface.events;
          line "end@,";
        );
    );
  with_output (file_base ^ "_server.ml") (make_wrappers ~internal `Server protocol);
  with_output (file_base ^ "_client.ml") (make_wrappers ~internal `Client protocol)
