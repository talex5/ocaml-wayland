open Internal

type ('a, 'v, 'role) t = ('a, 'role) Internal.proxy

type ('a, 'v, 'role) proxy = ('a, 'v, 'role) t

type ('v, 'role) generic = Proxy : ('a, 'v, 'role) t -> ('v, 'role) generic

module type TRACE = sig
  type role
  val outbound : ('a, 'v, role) t -> ('a, [`W]) Msg.t -> unit
  val inbound : ('a, 'v, role) t -> ('a, [`R]) Msg.t -> unit
end

let missing_dispatch _ = failwith "no handler registered!"

let missing_handler metadata = {
  metadata;
  user_data = S.No_data;
  dispatch = missing_dispatch;
}

(* Register [id] as a new object with a temporary dummy handler.
   Call [complete_accept] on the result before blocking. *)
let accept_new t ~version ~handler id =
  let conn = t.conn in
  let is_service_allocated_id = (Int32.unsigned_compare id 0xFF000000l >= 0) in
  begin match conn.role with
    | `Client -> assert is_service_allocated_id
    | `Server -> assert (not is_service_allocated_id)
  end;
  if Objects.mem id conn.objects then
    Fmt.failwith "An object with ID %ld already exists!" id;
  let t' = { id; version; conn; valid = false; handler; on_delete = Queue.create () } in
  conn.objects <- Objects.add id (Generic t') conn.objects;
  t'

let complete_accept t handler =
  assert (t.handler.dispatch == missing_dispatch);
  assert (not t.valid);
  t.handler <- handler;
  t.valid <- true

let cast_version t = (t : ('a, _, 'role) t :> ('a, _, 'role) t)

let version t = t.version

let metadata t = t.handler.metadata

let ty (type a) t =
  let (module M : Metadata.S with type t = a) = metadata t in
  M.T

let interface (type a) t =
  let (module M : Metadata.S with type t = a) = metadata t in
  M.interface

module Handler = struct
  type ('a, 'v, 'role) t = ('a, 'role) Internal.handler

  let interface (type a) t =
    let (module M : Metadata.S with type t = a) = t.metadata in
    M.interface

  let cast_version t = (t :> _ t)

  let v ?(user_data=S.No_data) metadata dispatch = { metadata; dispatch; user_data }

  let accept_new (type a) proxy (module M : Metadata.S with type t = a) id =
    accept_new proxy id ~version:proxy.version ~handler:(missing_handler (module M))

  let attach proxy handler =
    complete_accept proxy handler
end

let pp = pp_proxy

module Service_handler = struct
  type ('a, 'v, 'role) t = {
    handler : ('a, 'v, 'role) Handler.t;
    version : int32;
  }

  let interface t = Handler.interface t.handler

  let version t = t.version

  let cast_version t = (t :> _ t)

  let v ~version ?user_data metadata h =
    { version; handler = Handler.v ?user_data metadata h }

  let accept_new (type a) proxy id (module M : Metadata.S with type t = a) ~version : (a, [`Unknown], _) proxy =
    accept_new proxy id ~version ~handler:(missing_handler (module M))

  let attach (type a) (proxy : (a, _, _) proxy) { handler; version } =
    if proxy.version <> version then
      Fmt.invalid_arg "attach: expected %a to have version %ld, but got a handler for %ld" pp proxy proxy.version version;
    complete_accept proxy handler;
    proxy
end

let id t =
  assert t.valid;
  t.id

let id_opt = function
  | None -> 0l
  | Some t -> id t

let alloc t = Msg.alloc ~obj:t.id

let send (type a) (t:_ t) (msg : (a, [`W]) Msg.t) =
  t.conn.trace.outbound t msg;
  if t.valid then
    enqueue t.conn (Msg.cast msg)
  else
    Fmt.failwith "Attempt to use object %a after calling destructor!" pp t

let spawn_bind t {Service_handler.version; handler } =
  let conn = t.conn in
  let id = get_unused_id conn in
  let t' = { id; version; conn = t.conn; valid = true; handler; on_delete = Queue.create () } in
  conn.objects <- Objects.add id (Generic t') conn.objects;
  t'

let spawn t handler = spawn_bind t {Service_handler.version = t.version; handler}

let user_data (t:_ t) = t.handler.user_data

let invalidate t =
  assert t.valid;
  t.valid <- false

let add_root conn { Service_handler.version; handler } =
  assert (version = 1l);
  let display_proxy = { version; id = 1l; conn; valid = true; handler; on_delete = Queue.create () } in
  conn.objects <- Objects.add display_proxy.id (Generic display_proxy) conn.objects;
  display_proxy

let on_delete t fn =
  Queue.add fn t.on_delete

let delete t =
  let conn = t.conn in
  assert (conn.role = `Server);
  match Objects.find_opt t.id conn.objects with
  | Some (Generic t') when Obj.repr t == Obj.repr t' ->
    t.valid <- false;
    conn.objects <- Objects.remove t.id conn.objects;
    Internal.free_id conn t.id;
    let Generic display = Objects.find 1l conn.objects in
    let msg = alloc display ~op:1 ~ints:1 ~strings:[] ~arrays:[] in
    Msg.add_int msg t.id;
    send display msg;
    Queue.iter (fun f -> f ()) t.on_delete
  | _ -> Fmt.failwith "Object %a is not registered!" pp t

let delete_other proxy id =
  let conn = proxy.conn in
  match Objects.find_opt id conn.objects with
  | None -> Fmt.failwith "Object %ld does not exist!" id
  | Some (Generic proxy) ->
    if proxy.valid then Log.warn (fun f -> f "Object %a deleted while still marked as valid!" pp proxy);
    conn.objects <- Objects.remove id conn.objects;
    Internal.free_id conn id;
    Queue.iter (fun f -> f ()) proxy.on_delete

let unknown_event = Fmt.strf "<unknown event %d>"
let unknown_request = Fmt.strf "<unknown request %d>"

let lookup_other (t : _ t) id =
  match Objects.find_opt id t.conn.objects with
  | None -> Fmt.failwith "Proxy with ID %ld not found!" id
  | Some (Generic p) -> Proxy p

let wrong_type ~parent ~expected t =
  Fmt.failwith "Object %a referenced object %a, which should be of type %S but isn't'" pp parent pp t expected

let trace (type r) (module T : TRACE with type role = r) = {
  inbound = T.inbound;
  outbound = T.outbound;
}
