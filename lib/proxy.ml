open Internal

type ('a, 'v) t = 'a Internal.proxy

type ('a, 'v) proxy = ('a, 'v) t

type generic = Proxy : 'a Iface_reg.ty * ('a, [`Unknown]) t -> generic

let missing_dispatch _ = failwith "no handler registered!"

let missing_handler = {
  metadata = (module Iface_reg.Unknown);
  user_data = S.No_data;
  dispatch = missing_dispatch;
}

(* Register [id] as a new object with a temporary dummy handler.
   Call [complete_accept] on the result before blocking. *)
let accept_new t ~version ~handler id =
  let conn = t.conn in
  let is_service_allocated_id = (Int32.unsigned_compare id 0xFF000000l >= 0) in
  if conn.role = `Client then assert is_service_allocated_id
  else assert (not is_service_allocated_id);
  if Objects.mem id conn.objects then
    Fmt.failwith "An object with ID %ld already exists!" id;
  let t' = { id; version; conn; valid = false; handler } in
  conn.objects <- Objects.add id (Generic t') conn.objects;
  t'

let complete_accept t handler =
  assert (t.handler.dispatch == missing_dispatch);
  assert (not t.valid);
  t.handler <- handler;
  t.valid <- true

module Handler = struct
  type ('a, 'v) t = 'a Internal.handler

  let interface t =
    let (module M) = t.metadata in
    M.interface

  let cast_version t = (t :> _ t)

  let v ?(user_data=S.No_data) metadata dispatch = { metadata; dispatch; user_data }

  let accept_new proxy id =
    accept_new proxy id ~version:proxy.version ~handler:missing_handler

  let attach proxy handler =
    complete_accept proxy handler
end

module Service_handler = struct
  type ('a, 'v) t = {
    handler : ('a, 'v) Handler.t;
    version : int32;
  }

  let interface t = Handler.interface t.handler

  let version t = t.version

  let cast_version t = (t :> _ t)

  let v ~version ?user_data metadata h =
    { version; handler = Handler.v ?user_data metadata h }

  let accept_new proxy id ~interface ~version =
    let Iface_reg.Interface (ty, (module M : Metadata.S)) = interface in
    let handler = { metadata = (module M); user_data = S.No_data; dispatch = missing_dispatch } in
    let proxy' = accept_new proxy ~version ~handler id in
    Proxy (ty, proxy')

  let attach (proxy : ('a, _) proxy) { handler; version } =
    let (module M1) = proxy.handler.metadata in
    let (module M2) = handler.metadata in
    if M1.interface <> M2.interface then
      Fmt.invalid_arg "attach: expected interface %S, but got a handler for %S" M1.interface M2.interface;
    if proxy.version <> version then
      Fmt.invalid_arg "attach: expected %S to have version %ld, but got a handler for %ld" M1.interface proxy.version version;
    complete_accept proxy handler;
    (proxy :> ('a, _) proxy)
end

let pp = pp_proxy

let id t =
  assert t.valid;
  t.id

let alloc t = Msg.alloc ~obj:t.id

let send (t:_ t) (msg : ('a, [`W]) Msg.t) =
  Log.info (fun f ->
      let (module M) = t.handler.metadata in
      let outgoing_info =
        match t.conn.role with
        | `Client -> M.requests
        | `Server -> M.events
      in
      let msg_name, arg_info = outgoing_info (Msg.op msg) in
      f "@[<h>-> %a.%s %a@]"
               pp t
               msg_name
               (Msg.pp_args arg_info) msg;
    );
  if t.valid then
    enqueue t.conn (Msg.cast msg)
  else
    Fmt.failwith "Attempt to use object %a after calling destructor!" pp t

let spawn_bind t {Service_handler.version; handler } =
  let conn = t.conn in
  let id = get_unused_id conn in
  let t' = { id; version; conn = t.conn; valid = true; handler } in
  conn.objects <- Objects.add id (Generic t') conn.objects;
  t'

let spawn t handler = spawn_bind t {Service_handler.version = t.version; handler}

let user_data (t:_ t) = t.handler.user_data

let invalidate t =
  assert t.valid;
  t.valid <- false

let add_root conn { Service_handler.version; handler } =
  assert (version = 1l);
  let display_proxy = { version; id = 1l; conn; valid = true; handler } in
  conn.objects <- Objects.add display_proxy.id (Generic display_proxy) conn.objects;
  display_proxy

let delete t =
  let conn = t.conn in
  assert (conn.role = `Server);
  match Objects.find_opt t.id conn.objects with
  | Some (Generic t') when Obj.repr t == Obj.repr t' ->
    t.valid <- false;
    conn.objects <- Objects.remove t.id conn.objects;
    Internal.free_id conn t.id;
    let Generic display = Objects.find 1l conn.objects in
    let msg = alloc display ~op:1 ~ints:1 ~strings:[] in
    Msg.add_int msg t.id;
    send display msg
  | _ -> Fmt.failwith "Object %a is not registered!" pp t

let delete_other proxy id =
  let conn = proxy.conn in
  let old = conn.objects in
  conn.objects <- Objects.remove id conn.objects;
  assert (conn.objects != old);
  Internal.free_id conn id

let unknown_event = Fmt.strf "<unknown event %d>"
let unknown_request = Fmt.strf "<unknown request %d>"

let lookup_other_unsafe ~interface (t : ('a, _) t) id : ('b, _) t =
  match Objects.find_opt id t.conn.objects with
  | None -> Fmt.failwith "Proxy with ID %ld not found!" id
  | Some (Generic p) ->
    let (module M) = p.handler.metadata in
    if M.interface <> interface then
      Fmt.failwith "Object %a referenced object %a, which should be of type %S but isn't'" pp t pp p interface;
    (* If the interfaces are the same then the type must be too, because it's generated directly from the name. *)
    (Obj.magic p : _ t)
