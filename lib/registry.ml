open Wayland_client

type entry = {
  name : int32;
  version : int32;
}

type t = {
  db : (string, entry) Hashtbl.t;
  registry : [`V1] Wl_registry.t;
}

type ('a, 'v) handler = <
  ('a, 'v, [`Client]) Proxy.Service_handler.t;
  bind_version : 'v;
>

let add db ~name ~interface ~version =
  Hashtbl.add db interface { name; version }

let remove db ~name =
  db |> Hashtbl.filter_map_inplace (fun _iface v ->
      if v.name = name then None
      else Some v
    )

let of_display d =
  let db = Hashtbl.create 20 in
  let registry = Wl_display.get_registry (Client.wl_display d) @@ object
      inherit [_] Wl_registry.v1
      method on_global _ = add db
      method on_global_remove _ = remove db
    end
  in
  Client.sync d;
  { db; registry }

let get t = Hashtbl.find_all t.db

let get_exn t interface =
  match get t interface with
  | [] -> Fmt.failwith "Required interface %S not found in registry!" interface
  | e :: _ -> e

let bind t handler =
  let iface = Proxy.Service_handler.interface handler in
  let min_version = handler#min_version in
  let {name; version = service_max_version} = get_exn t iface in
  if service_max_version < min_version then
    Fmt.failwith "Can't use version %ld of %s; registry only supports <= %ld" min_version iface service_max_version;
  Wl_registry.bind t.registry ~name (handler, min handler#max_version service_max_version)

let wl_registry t = t.registry
