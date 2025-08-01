open Schema

let module_name = String.capitalize_ascii

let full_module_name (proto : Protocol.t) (iface : Interface.t) =
  Fmt.str "%s_proto.%s" (String.capitalize_ascii proto.name) (module_name iface.name)

(* Normally the child has the same version as the parent. But some interfaces don't
   follow this rule (wl_callback, wl_buffer, wl_region) and remain at v1. *)
let is_v1 = function
  | "wl_callback" | "wl_buffer" | "wl_region" -> true
  | _ -> false

let mangle name =
  let name =
    name |> String.map @@ function
    | '-' -> '_'
    | x -> x
  in
  match name with
  | "type"
  | "method"
  | "handlers"
  | "h"
  | "done" -> name ^ "_"
  | x -> x

let pp_role f = function
  | `Client -> Fmt.string f "[`Client]"
  | `Server -> Fmt.string f "[`Server]"

let pp_poly f name =
  Fmt.pf f "[`%s]" (String.capitalize_ascii (mangle name))

let pp_enum_module (iface : Interface.t) f (arg:Arg.t) =
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
  Fmt.pf f "Imports.%a"
    Fmt.(list ~sep:(any ".") string) enum

let pp_tvars f = function
  | 0 -> ()
  | n ->
    for i = 0 to n - 1 do
      if i > 0 then Fmt.sp f ();
      Fmt.pf f "'x%d" i;
    done;
    Fmt.string f ". "

let pp_type ~role ~next_tvar iface f (arg:Arg.t) =
  begin match arg.ty with
    | `Uint | `Int when arg.enum <> None -> Fmt.pf f "%a.t" (pp_enum_module iface) arg
    | `Uint | `Int -> Fmt.string f "int32"
    | `String -> Fmt.string f "string"
    | `Array -> Fmt.string f "string"
    | `Object None -> Fmt.string f "int32"
    | `Object (Some i) -> Fmt.pf f "(%a, [> Imports.%s.versions], %a) Proxy.t" pp_poly i (module_name i) pp_role role
    | `New_ID None ->
      let t = !next_tvar in
      next_tvar := t + 1;
      Fmt.pf f "('x%d, [`Unknown], %a) Proxy.t" t pp_role role
    | `New_ID (Some i) when is_v1 i -> Fmt.pf f "(%a, [`V1], %a) Proxy.t" pp_poly i pp_role role
    | `New_ID (Some i) -> Fmt.pf f "(%a, 'v, %a) Proxy.t" pp_poly i pp_role role
    | `Fixed -> Fmt.string f "Fixed.t"
    | `FD -> Fmt.string f "Unix.file_descr"
  end;
  if arg.allow_null then Fmt.string f " option"

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

let pp_arg ~role ~next_tvar iface f arg =
  if named_argument arg then
    Fmt.pf f "%s:%a" (mangle arg.name) (pp_type ~role ~next_tvar iface) arg
  else
    pp_type ~role ~next_tvar iface f arg

let pp_sig ~role ~next_tvar iface f = function
  | [] -> Fmt.string f "unit"
  | args -> Fmt.(list ~sep:(any " ->@ ") (pp_arg ~role ~next_tvar iface) ++ any " ->@ unit") f args

let pp_args ~role ~with_types =
  let pp_arg f arg =
    if named_argument arg then Fmt.string f "~";
    let m = mangle arg.name in
    match arg.ty with
    | `Object (Some interface) when with_types ->
      Fmt.pf f "(%s:(%a, _, %a) Proxy.t%s)" m pp_poly interface pp_role role
        (if arg.allow_null then " option" else "")
    | `New_ID (Some interface) when with_types ->
      let v = if is_v1 interface then "[`V1]" else "'v" in
      Fmt.pf f "(%s:(%a, %s, %a) #Proxy.Handler.t)" m pp_poly interface v pp_role role
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
  | `Object interface -> Fmt.str "`Object (%a)" Fmt.Dump.(option string) interface
  | `New_ID interface -> Fmt.str "`New_ID (%a)" Fmt.Dump.(option string) interface
  | `Fixed -> "`Fixed"
  | `FD -> "`FD"

let pp_arg_info =
  let pp_arg f (arg : Arg.t) =
    Fmt.pf f "%S, %s" arg.name (variant_of_ty arg)
  in
  Fmt.(list ~sep:semi) pp_arg

let pp_versions f (min, max) =
  Fmt.(list ~sep:(any " | ") (fmt "`V%d"))
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

let fix_quotes s =
  let rec unquoted i =
    match String.index_from_opt s i '"' with
    | None -> s         (* Correctly quoted string *)
    | Some i -> quoted (i + 1)
  and quoted i =
    match String.index_from_opt s i '"' with
    | None -> s ^ "\""  (* Incorrectly quoted string *)
    | Some i -> unquoted (i + 1)
  in
  unquoted 0

let fix_comments =
  let re_end_comment = Str.regexp_string "*)" in
  Str.global_replace re_end_comment "*[])"

let comment f = function
  | None -> ()
  | Some (d : Description.t) ->
    let full =
      d.full
      |> fix_quotes
      |> fix_comments
      |> String.split_on_char '\n'
      |> List.map String.trim
      |> trim_lines
    in
    Fmt.pf f "@,(** @[<v>%s.@,@,%a@] *)" (String.capitalize_ascii d.summary) Fmt.(list ~sep:cut string) full

let pp_strings f args =
  args
  |> List.filter_map (fun (a : Arg.t) ->
      match a.ty with
      | `New_ID None -> Some (Fmt.str "(Some (Proxy.Service_handler.interface (fst %s)))" (mangle a.name))
      | `String ->
        if a.allow_null then Some (mangle a.name)
        else Some (Fmt.str "(Some %s)" (mangle a.name))
      | _ -> None
    )
  |> Fmt.(list ~sep:semi string) f

let pp_arrays f args =
  args
  |> List.filter_map (fun (a : Arg.t) ->
      match a.ty with
      | `Array -> Some (mangle a.name)
      | _ -> None
    )
  |> Fmt.(list ~sep:semi string) f

let rec root_interface ~parents (interface : Interface.t) =
  match Parent.parent parents interface with
  | None -> interface
  | Some parent ->
    if parent.version < interface.version then (
      Fmt.failwith "Interface %S has version %d, which is less that its child %S at %d!"
        parent.name parent.version
        interface.name interface.version
    );
    root_interface ~parents parent

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
        Fmt.pf f "@,let %s = %ldl" (mangle e.name) e.value
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
    Fmt.pf f "@]";
    Fmt.pf f "@,@,@[<v2>let of_int32 = function";
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

let pp_msg_handler_sig ~role ~iface ~pp_self f (msg : Message.t) =
  let next_tvar = ref 0 in
  let _args = Fmt.str "@[%a@]" (pp_sig ~role ~next_tvar iface) msg.args in
  let n_tvars = !next_tvar in
  next_tvar := 0;
  Fmt.pf f "@[@[%a@]%t %a@]@,"
    pp_tvars n_tvars
    (fun f -> if msg.ty = `Normal || role = `Server then (pp_self f; Fmt.string f " ->"))
    (pp_sig ~role ~next_tvar iface) msg.args

let make_wrappers ~opens ~internal role (protocol : Protocol.t) f =
  let parents = Parent.index protocol in
  let line fmt = Fmt.pf f ("@," ^^ fmt) in
  line {|[@@@@@@ocaml.warning "-27-34"]|};
  line "@[<v2>open struct";
  line "@[<v2>module Imports = struct";
  line "include %s_proto" (module_name protocol.name);
  List.iter (line "include %s") opens;
  Fmt.pf f "@]@,end";
  let _incoming, _outgoing =
    match role with
    | `Client -> "event", "request"
    | `Server -> "request", "event"
  in
  if not internal then (
    line "@,module Proxy = Wayland.Proxy";
    line "module Msg = Wayland.Msg";
    line "module Fixed = Wayland.Fixed";
    line "module Iface_reg = Wayland.Iface_reg";
    line "module S = Wayland.S";
  );
  Fmt.pf f "@]@,end";
  line "";
  protocol.interfaces |> List.iter (fun (iface : Interface.t) ->
      let root = root_interface ~parents iface in
      let n_versions = min iface.version root.version in
      let is_service = root == iface in
      let versions = get_versions ~n_versions iface in
      line "";
      comment f iface.description;
      line "@[<v2>module %s = struct" (module_name iface.name);
      line "type 'v t = (%a, 'v, %a) Proxy.t" pp_poly iface.name pp_role role;
      Fmt.list (pp_enum_link protocol iface) f iface.enums;
      let have_incoming = ref false in
      let () =
        let errors =
          match role with
          | `Client -> []
          | `Server ->
             let enum_is_error (i: Enum.t): bool = i.name = "error" in
             match List.find_all enum_is_error iface.enums with
             | [] -> []
             | [e] -> e.entries
             | _ :: _ :: _ -> failwith "Multiple error enums"
        in
        if errors <> [] then (
          line "module Errors = struct";
          (* Posting errors *)
          let pp_post_error (error : Entry.t) =
            line "@[<v2>let %s (proxy: 'v t) ~(message: string): 'a =" error.name;
            line "@[<v2>Proxy.post_error proxy ~code:%dl ~message@]@]" (Int32.to_int error.value)
          in
          List.iter pp_post_error errors;
          line "end"
        )
      in

      versions |> List.iter (fun (group : version_group) ->
          line "";
          line "(** {2 Version @[<h>%a@]} *)" Fmt.(list ~sep:comma int) group.versions;
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
              line "@[<v2>let %s (_t:([< %a] as 'v) t) @[<h>%a@] ="
                (mangle msg.name) pp_versions (msg.since, n_versions) (pp_args ~role ~with_types:true) msg.args;
              new_ids |> List.iter (fun (arg : Arg.t) ->
                  let ty = match arg.ty with `New_ID x -> x | _ -> assert false in
                  let v1 = match ty with Some i -> is_v1 i | None -> false in
                  let m = mangle arg.name in
                  line "let __%s = Proxy.spawn%s %s %s in"
                    m
                    (match ty with None -> "_bind" | _ -> "")
                    (if v1 then "(Proxy.cast_version _t)" else "_t")
                    m
                );
              line "let _msg = Proxy.alloc _t ~op:%d ~ints:%d ~strings:[%a] ~arrays:[%a] in"
                i (List.length msg.args + extra_version_fields)
                pp_strings msg.args
                pp_arrays msg.args;
              msg.args |> List.iter (fun (arg : Arg.t) ->
                  let m = mangle arg.name in
                  match arg.ty with
                  | `New_ID None ->
                    line "Msg.add_string _msg (Proxy.Service_handler.interface (fst %s));" m;
                    line "Msg.add_int _msg (snd %s);" m;
                    line "Msg.add_int _msg (Proxy.id __%s);" m
                  | `New_ID (Some _) ->
                    line "Msg.add_int _msg (Proxy.id __%s);" m
                  | `Object _ ->
                    line "Msg.add_int _msg (Proxy.id%s %s);"
                      (if arg.allow_null then "_opt" else "")
                      m
                  | `Int | `Uint when arg.enum <> None ->
                    line "Msg.add_int _msg (%a.to_int32 %s);"
                      (pp_enum_module iface) arg m
                  | _ ->
                    line "Msg.add_%a%s _msg %s;"
                      pp_type_getter arg.ty
                      (if arg.allow_null then "_opt" else "")
                      m
                );
              line "Proxy.send _t _msg";
              if msg.ty = `Destructor then
                Fmt.pf f ";@,Proxy.shutdown_send _t";
              if new_ids = [] then Fmt.pf f "@]"
              else (
                Fmt.pf f ";";
                let arg_name f arg = Fmt.pf f "__%s" (mangle arg.Arg.name) in
                line "%a@]"
                  Fmt.(list ~sep:comma arg_name) new_ids
              )
            );
          line "";
        );
      (* Handlers.
         We define a hidden "unsafe" handler class that ignores versions, and then make the public
         handlers inherit from that, specialising the types to the ones we want.
         For example:
         - [Wl_data_offer.v1.on_accept] needs to accept a v1-or-later proxy.
         - [Wl_data_offer.v2.on_accept] needs to accept a v2-or-later proxy.
         - [Wl_data_offer.v1.on_set_actions] only needs to handle v3 proxies,
           because [set_actions] is a v3-only operation. *)
      let msgs_in, _msgs_out =
        match role with
        | `Client -> iface.events, iface.requests
        | `Server -> iface.requests, iface.events
      in
      let opt_virtual = if msgs_in <> [] then "virtual " else "" in
      line "(**/**)";
      line "@[<v2>class %s['v] _handlers_unsafe = object (_self : (_, 'v, _) #Proxy.Handler.t)"
        opt_virtual;
      line "method user_data = S.No_data";
      line "method metadata = (module %s)" (full_module_name protocol iface);
      line "method max_version = %dl" n_versions;
      line "";
      msgs_in |> List.iter (fun (msg : Message.t) ->
          let pp_self f = Fmt.string f "[> ] t" in
          line "method private virtual on_%s : %a" msg.name (pp_msg_handler_sig ~role ~iface ~pp_self) msg;
        );
      line "";
      line "@[<v2>method dispatch (_proxy : 'v t) _msg =";
      line "let _proxy = Proxy.cast_version _proxy in";
      line "match Msg.op _msg with";
      msgs_in |> List.iteri (fun i (msg : Message.t) ->
          line "@[<v2>| %d ->" i;
          msg.args |> List.iteri (fun i (arg : Arg.t) ->
              let m = mangle arg.name in
              begin match arg.ty with
                | `Int | `Uint when arg.enum <> None ->
                  line "@[<v2>let %s = Msg.get_int _msg |> %a.of_int32 in@]" m (pp_enum_module iface) arg
                | `New_ID None ->
                  line "let (module M%d : Metadata.S) = Msg.get_string _msg |> Iface_reg.lookup in" i;
                  line "@[<v2>let %s =" m;
                  line "let version = Msg.get_int _msg in";
                  line "let id = Msg.get_int _msg in";
                  line "Proxy.Service_handler.accept_new _proxy id (module M%d) ~version@]@,in" i;
                | `New_ID (Some i) ->
                  line "@[<v2>let %s : (%a, _, _) Proxy.t =@ Msg.get_int _msg |> Proxy.Handler.accept_new _proxy (module Imports.%s) in@]"
                    m pp_poly i
                    (module_name i)
                | `Object (Some i) when arg.allow_null ->
                  line "@[<v2>let %s : (%a, _, _) Proxy.t option =" m pp_poly i;
                  line "match Msg.get_int _msg with";
                  line "| 0l -> None";
                  line "@[<v2>| id ->";
                  line "let Proxy.Proxy p = Proxy.lookup_other _proxy id in";
                  line "match Proxy.ty p with";
                  line "| Imports.%s.T -> Some p" (module_name i);
                  line "| _ -> Proxy.wrong_type ~parent:_proxy ~expected:%S p@]" i;
                  line "in@]"
                | `Object (Some i) ->
                  line "@[<v2>let %s : (%a, _, _) Proxy.t =" m pp_poly i;
                  line "let Proxy.Proxy p = Msg.get_int _msg |> Proxy.lookup_other _proxy in";
                  line "match Proxy.ty p with";
                  line "| Imports.%s.T -> p" (module_name i);
                  line "| _ -> Proxy.wrong_type ~parent:_proxy ~expected:%S p" i;
                  line "in@]"
                | _ ->
                  line "@[<v2>let %s = Msg.get_%a%s _msg in@]" m pp_type_getter arg.ty
                    (if arg.allow_null then "_opt" else "");
              end;
            );
          if msg.ty = `Destructor then
            line "Proxy.shutdown_recv _proxy;";
          line "_self#on_%s %s@[%a@]@]"
            msg.name
            (if msg.ty = `Normal || role = `Server then "_proxy " else "")
            (pp_args ~role ~with_types:false) msg.args;
        );
      line "| _ -> assert false@]";
      Fmt.pf f "@]@,end";
      line "(**/**)";
      line "";
      line "(** {2 Handlers}";
      line "    Note: Servers will always want to use [v1].";
      line " *)";
      line "";
      for v = 1 to n_versions do
        line "";
        line "(** Handler for a proxy with version >= %d. *)" v;
        line "@[<v2>class %s['v] v%d = object (_ : (_, 'v, _) #Proxy.Service_handler.t)" opt_virtual v;
        line "(**/**)";
        line "inherit [[< %a] as 'v] _handlers_unsafe" pp_versions (v, n_versions);
        line "(**/**)";
        msgs_in |> List.iter (fun (msg : Message.t) ->
            let pp_self f =
              Fmt.pf f "[> %a] t" pp_versions (max v msg.since, n_versions)
            in
            line "method private virtual on_%s : %a" msg.name (pp_msg_handler_sig ~role ~iface ~pp_self) msg;
            comment f msg.description;
            Fmt.cut f ()
          );
        line "method min_version = %dl" v;
        if is_service then
          line "method bind_version : [`V%d] = `V%d" v v;
        Fmt.pf f "@]@,end";
      done;
      Fmt.pf f "@]@,end"; (* Interface *)
    )

let output ~opens ~internal (protocol : Protocol.t) =
  let file_base = mangle protocol.name in
  with_output (file_base ^ "_proto.ml") (fun f ->
      let line fmt = Fmt.pf f ("@," ^^ fmt) in
      if not internal then (
        line "@[<v2>open struct";
        line "module Proxy = Wayland.Proxy";
        line "module Iface_reg = Wayland.Iface_reg";
        line "module Metadata = Wayland.Metadata";
        Fmt.pf f "@]@,end@,";
      );
      protocol.interfaces |> List.iter (fun (iface : Interface.t) ->
          line "@[<v2>module %s = struct" (module_name iface.name);
          line "type t = %a" pp_poly iface.name;
          line "type _ Metadata.ty += T : %a Metadata.ty" pp_poly iface.name;
          line "type versions = [%a]" pp_versions (1, iface.version);
          line "let interface = %S" iface.name;
          line "let version = %ul" iface.version;
          Fmt.(list ~sep:cut) pp_enum f iface.enums;
          line "";
          line "@[<v2>let requests = %a@]@," (op_info "request") iface.requests;
          line "@[<v2>let events = %a@]@," (op_info "event") iface.events;
          Fmt.pf f "@]@,end";
          line "let () = Iface_reg.register (module %s)@," (module_name iface.name);
        );
    );
  with_output (file_base ^ "_server.ml") (make_wrappers ~opens ~internal `Server protocol);
  with_output (file_base ^ "_client.ml") (make_wrappers ~opens ~internal `Client protocol)
