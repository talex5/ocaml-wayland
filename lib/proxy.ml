open Internal

type ('a, 'v, 'role) t = ('a, 'role) Internal.proxy

type ('a, 'v, 'role) proxy = ('a, 'v, 'role) t

type ('v, 'role) generic = Proxy : ('a, 'v, 'role) t -> ('v, 'role) generic

module type TRACE = sig
  type role
  val outbound : ('a, 'v, role) t -> ('a, [`W]) Msg.t -> unit
  val inbound : ('a, 'v, role) t -> ('a, [`R]) Msg.t -> unit
end

let pp = pp_proxy

let missing_dispatch _ = failwith "no handler registered!"

let missing_handler metadata = object (_ : _ handler)
  method metadata = metadata
  method user_data = S.No_data
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
  (*   assert (t.handler.dispatch == missing_dispatch); *)
  t.handler <- handler

let cast_version t = (t : ('a, _, 'role) t :> ('a, _, 'role) t)

let version t = t.version

let metadata t = t.handler#metadata

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

  let v ?(user_data=S.No_data) metadata dispatch = 
    object
      method metadata = metadata
      method dispatch = dispatch
      method user_data = user_data
    end

  let accept_new (type a) proxy (module M : Metadata.S with type t = a) id =
    accept_new proxy id ~version:proxy.version ~handler:(missing_handler (module M))

  let attach proxy handler =
    complete_accept proxy (handler :> _ handler)
end

module Service_handler = struct
  class type ['a, 'v, 'role] t = object
    inherit ['a, 'v, 'role] Handler.t
    method version : int32
  end

  let interface t = Handler.interface t

  let version t = t#version

  let cast_version t = (t :> _ t)

  let v ~version ?user_data metadata h =
    let handler = Handler.v ?user_data metadata h in
    object
      method metadata = handler#metadata
      method dispatch = handler#dispatch
      method user_data = handler#user_data
      method version = version
    end

  let accept_new (type a) proxy id (module M : Metadata.S with type t = a) ~version : (a, [`Unknown], _) proxy =
    accept_new proxy id ~version ~handler:(missing_handler (module M))

  let attach (type a) (proxy : (a, _, _) proxy) (t:(_, _, _) #t) =
    if proxy.version <> t#version then
      Fmt.invalid_arg "attach: expected %a to have version %ld, but got a handler for %ld" pp proxy proxy.version t#version;
    complete_accept proxy (t :> _ handler);
    proxy
end

let id t = t.id

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

let spawn_bind t (handler : (_, _, _) #Service_handler.t) =
  let conn = t.conn in
  let id = get_unused_id conn in
  let t' = make_proxy id ~version:handler#version ~conn:t.conn ~handler:(handler :> _ handler) in
  conn.objects <- Objects.add id (Generic t') conn.objects;
  t'

let spawn t (handler : (_, _, _) #Handler.t) =
  let conn = t.conn in
  let id = get_unused_id conn in
  let t' = make_proxy id ~version:t.version ~conn:t.conn ~handler in
  conn.objects <- Objects.add id (Generic t') conn.objects;
  t'

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

let add_root conn (handler : (_, _, _) #Service_handler.t) =
  assert (handler#version = 1l);
  let display_proxy = make_proxy 1l ~version:handler#version ~conn ~handler in
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
    Queue.iter (fun f -> f ()) t.on_delete
  | _ -> Fmt.failwith "Object %a is not registered!" pp t

let delete_other proxy id =
  let conn = proxy.conn in
  match Objects.find_opt id conn.objects with
  | None -> Fmt.failwith "Object %ld does not exist!" id
  | Some (Generic proxy) ->
    proxy.can_recv <- false;
    conn.objects <- Objects.remove id conn.objects;
    Internal.free_id conn id;
    Queue.iter (fun f -> f ()) proxy.on_delete

let unknown_event = Fmt.strf "<unknown event %d>"
let unknown_request = Fmt.strf "<unknown request %d>"

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
