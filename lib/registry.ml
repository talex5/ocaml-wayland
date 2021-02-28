open Lwt.Syntax
open Wayland_client

type entry = {
  name : int32;
  version : int32;
}

type t = {
  db : (string, entry) Hashtbl.t;
  registry : [`V1] Wl_registry.t;
}

let add db ~name ~interface ~version =
  Hashtbl.add db interface { name; version }

let remove db ~name =
  db |> Hashtbl.filter_map_inplace (fun _iface v ->
      if v.name = name then None
      else Some v
    )

let of_display d =
  let db = Hashtbl.create 20 in
  let registry = Wl_display.get_registry (Display.wl_display d) @@
    Wl_registry.v1 @@ object
      method on_global _ = add db
      method on_global_remove _ = remove db
    end
  in
  let* () = Display.sync d in
  Lwt.return { db; registry }

let get t = Hashtbl.find_all t.db

let get_exn t interface =
  match get t interface with
  | [] -> Fmt.failwith "Required interface %S not found in registry!" interface
  | e :: _ -> e

let bind t handler =
  let iface = Proxy.Service_handler.interface handler in
  let handler_version = Proxy.Service_handler.version handler in
  let {name; version} = get_exn t iface in
  if version < handler_version then
    Fmt.failwith "Can't use version %ld of %s; registry only supports <= %ld" handler_version iface version;
  Wl_registry.bind t.registry ~name handler

let wl_registry t = t.registry
