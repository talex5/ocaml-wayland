open Internal

type ('a, 'v, 'role) t = ('a, 'v, 'role) versioned_proxy

type ('a, 'v, 'role) proxy = ('a, 'v, 'role) t

type ('v, 'role) generic = Proxy : ('a, 'v, 'role) t -> ('v, 'role) generic

module type TRACE = sig
  type role
  val outbound : ('a, 'v, role) t -> ('a, [`W]) Msg.t -> unit
  val inbound : ('a, 'v, role) t -> ('a, [`R]) Msg.t -> unit
end

let pp = pp_proxy

let missing_dispatch _ = failwith "no handler registered!"

type (_, _) S.user_data += Missing_handler

let missing_handler metadata = object (_ : _ handler)
  method metadata = metadata
  method user_data = Missing_handler
  method dispatch = missing_dispatch
end

let make_proxy ~conn ~version ~handler id =
  { conn; id; version; can_send = true; can_recv = true; handler = (handler :> _ handler); on_delete = Queue.create () }

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
    Fmt.failwith "An object with ID %lu already exists!" id;
  let t' = make_proxy id ~version ~conn ~handler in
  conn.objects <- Objects.add id (Generic t') conn.objects;
  t'

let complete_accept t handler =
  assert (t.handler#user_data == Missing_handler);
  t.handler <- handler

let cast_version t = (t : ('a, _, 'role) t :> ('a, _, 'role) t)

let version t = t.version

let metadata t = t.handler#metadata

let can_send t = t.can_send

let transport_up t = t.conn.transport#up

let ty (type a) t =
  let (module M : Metadata.S with type t = a) = metadata t in
  M.T

let interface (type a) t =
  let (module M : Metadata.S with type t = a) = metadata t in
  M.interface

module Handler = struct
  class type ['a, 'v, 'role] t = object
    method user_data : ('a, 'role) S.user_data
    method metadata : (module Metadata.S with type t = 'a)
    method dispatch : ('a, 'v, 'role) proxy -> ('a, [`R]) Msg.t -> unit
  end

  let interface (type a) (t : (_, _ ,_) #t) =
    let (module M : Metadata.S with type t = a) = t#metadata in
    M.interface

  let cast_version t = (t :> _ t)

  let accept_new (type a) proxy (module M : Metadata.S with type t = a) id =
    accept_new proxy id ~version:proxy.version ~handler:(missing_handler (module M))

  let attach proxy handler =
    complete_accept proxy (handler :> _ handler)
end

module Service_handler = struct
  class type ['a, 'v, 'role] t = object
    inherit ['a, 'v, 'role] Handler.t
    method min_version : int32
    method max_version : int32
  end

  let interface t = Handler.interface t

  let min_version t = t#min_version

  let cast_version t = (t :> _ t)

  let accept_new (type a) proxy id (module M : Metadata.S with type t = a) ~version : (a, [`Unknown], _) proxy =
    accept_new proxy id ~version ~handler:(missing_handler (module M))

  let attach (type a) (proxy : (a, _, _) proxy) (t:(_, _, _) #t) =
    if proxy.version < t#min_version then
      Fmt.invalid_arg "attach: %a has version %ld, but handler requires version >= %ld" pp proxy proxy.version t#min_version;
    if proxy.version > t#max_version then
      Fmt.invalid_arg "attach: %a has version %ld, but handler requires version <= %ld" pp proxy proxy.version t#max_version;
    complete_accept proxy (t :> _ handler)

  let attach_proxy proxy t =
    attach proxy t;
    proxy
end

let id t =
  if t.can_send then t.id
  else Fmt.invalid_arg "Attempt to use %a after destroying it" pp t

let id_opt = function
  | None -> 0l
  | Some t -> id t

let alloc t = Msg.alloc ~obj:t.id

let send (type a) (t:_ t) (msg : (a, [`W]) Msg.t) =
  t.conn.trace.outbound t msg;
  if t.can_send then
    enqueue t.conn (Msg.cast msg)
  else
    Fmt.failwith "Attempt to use object %a after calling destructor!" pp t

let spawn_generic t ~version (handler : (_, _, _) #Handler.t) =
  let conn = t.conn in
  let id = get_unused_id conn in
  let t' = make_proxy id ~version ~conn:t.conn ~handler:(handler :> _ handler) in
  conn.objects <- Objects.add id (Generic t') conn.objects;
  t'

let spawn t (handler : (_, _, _) #Handler.t) =
  spawn_generic t ~version:t.version handler

let spawn_bind (t : (_, _, 'role) t) ((handler : ('a, 'v, 'role) #Service_handler.t), version) : ('a, 'v, 'role) t =
  let min_version = handler#min_version in
  let max_version = handler#max_version in
  if version < min_version then
    Fmt.failwith "Can't ask for %s version %ld when handler requires version >= %ld"
      (Handler.interface handler) version min_version;
  if version > max_version then
    Fmt.failwith "Can't ask for %s version %ld when handler requires version <= %ld"
      (Handler.interface handler) version max_version;
  spawn_generic t ~version handler

let user_data (t:_ t) = t.handler#user_data

let shutdown_send t =
  if t.can_send then
    t.can_send <- false
  else
    Fmt.failwith "%a already shut down!" pp t

let shutdown_recv t =
  if t.can_recv then
    t.can_recv <- false
  else
    Fmt.failwith "%a already shut down!" pp t

let add_root conn (handler : (_, _, _) #Handler.t) =
  let display_proxy = make_proxy 1l ~version:1l ~conn ~handler in
  conn.objects <- Objects.add display_proxy.id (Generic display_proxy) conn.objects;
  display_proxy

let on_delete t fn =
  Queue.add fn t.on_delete

let delete t =
  let conn = t.conn in
  match Objects.find_opt t.id conn.objects with
  | Some (Generic t') when Obj.repr t == Obj.repr t' ->
    t.can_recv <- false;
    t.can_send <- false;
    conn.objects <- Objects.remove t.id conn.objects;
    Internal.free_id conn t.id;
    begin match conn.role with
      | `Client -> ()
      | `Server ->
        if not (id_allocated_by_us conn t.id) then (
          let Generic display = Objects.find 1l conn.objects in
          let msg = alloc display ~op:1 ~ints:1 ~strings:[] ~arrays:[] in
          Msg.add_int msg t.id;
          send display msg
        )
    end;
    Queue.iter (fun f -> f ()) t.on_delete;
    Queue.clear t.on_delete
  | _ -> Fmt.failwith "Object %a is not registered!" pp t

let delete_other proxy id =
  let conn = proxy.conn in
  match Objects.find_opt id conn.objects with
  | None -> Fmt.failwith "Object %ld does not exist!" id
  | Some (Generic proxy) ->
    proxy.can_recv <- false;
    conn.objects <- Objects.remove id conn.objects;
    Internal.free_id conn id;
    Queue.iter (fun f -> f ()) proxy.on_delete;
    Queue.clear proxy.on_delete

let unknown_event = Fmt.str "<unknown event %d>"
let unknown_request = Fmt.str "<unknown request %d>"

let lookup_other (t : _ t) id =
  match Objects.find_opt id t.conn.objects with
  | None -> Fmt.failwith "Proxy with ID %ld not found!" id
  | Some (Generic p) ->
    if p.can_recv then
      Proxy p
    else
      Fmt.failwith "Message referred to object %a, which cannot receive further messages" pp p

let wrong_type ~parent ~expected t =
  Fmt.failwith "Object %a referenced object %a, which should be of type %S but isn't'" pp parent pp t expected

let trace (type r) (module T : TRACE with type role = r) = {
  inbound = T.inbound;
  outbound = T.outbound;
}

let pp_transport f t =
  t.conn.transport#pp f
