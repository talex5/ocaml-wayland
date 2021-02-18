open Internal

type ('a, 'v) t = 'a Internal.proxy

type ('a, 'v, 'vspawn, 'vbind) handler = {
  metadata : (module Metadata.S);
  version : int32;
  handler : ('a, 'v) t -> ('a, [`R]) Msg.t -> unit;
}

let handler metadata ~version handler = { metadata; version; handler }

let pp = pp_proxy

let id t =
  assert t.valid;
  t.id

let interface (t:_ t) =
  let (module M) = t.metadata in
  M.interface

let version (t : _ t) = t.version

let cast_version x = (x :> _ handler)

let alloc t = Msg.alloc ~obj:t.id

let send (t:_ t) (msg : ('a, [`W]) Msg.t) =
  Log.info (fun f ->
      let (module M) = t.metadata in
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

let spawn_bind t {metadata; version; handler} =
  let conn = t.conn in
  let id = get_unused_id conn in
  let t' = { id; version; conn = t.conn; valid = true; metadata } in
  conn.objects <- Objects.add id (Handler (t', handler)) conn.objects;
  t'

let spawn t handler = spawn_bind t {handler with version = t.version}

let invalidate t =
  assert t.valid;
  t.valid <- false

let add_root conn { metadata; version; handler } =
  assert (version = 1l);
  let display_proxy = { metadata; version; id = 1l; conn; valid = true } in
  conn.objects <- Objects.add display_proxy.id (Handler (display_proxy, handler)) conn.objects;
  display_proxy

let delete proxy id =
  let conn = proxy.conn in
  let old = conn.objects in
  conn.objects <- Objects.remove id conn.objects;
  assert (conn.objects != old);
  Internal.free_id conn id

let unknown_event = Fmt.strf "<unknown event %d>"
let unknown_request = Fmt.strf "<unknown request %d>"

let handler_interface (h : _ handler) =
  let (module M) = h.metadata in
  M.interface

let handler_version (h : _ handler) =
  h.version
