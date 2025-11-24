open Eio.Std

module Objects = Map.Make(Int32)

type 'role connection = {
  sw : Switch.t;
  transport : S.transport;
  mutable unpause : unit -> unit;
  role : 'role;
  mutable objects : 'role generic_proxy Objects.t;
  mutable free_ids : int32 list;
  mutable next_id : int32;
  incoming_fds : Unix.file_descr Queue.t;
  outbox : (unit, [`W]) Msg.t Queue.t;          (* The transmit thread is running whenever this is non-empty. *)
  trace : 'role tracer;
} and ('a, 'role) proxy = {
  id : int32;
  conn : 'role connection;
  version : int32;
  on_delete : (unit -> unit) Queue.t;
  mutable handler : ('a, 'role) handler;
  mutable can_send : bool;  (* False once we've called a destructor *)
  mutable can_recv : bool;  (* False once the peer has called a destructor. *)
}
and 'role generic_proxy = Generic : ('a, 'role) proxy -> 'role generic_proxy
and ('a, 'role) handler = <
  user_data : ('a, 'role) S.user_data;
  metadata : (module Metadata.S with type t = 'a);
  dispatch : ('a, 'role) proxy -> ('a, [`R]) Msg.t -> unit;
>
and 'role tracer = {
  outbound : 'a. ('a, 'role) proxy -> ('a, [`W]) Msg.t -> unit;
  inbound : 'a. ('a, 'role) proxy -> ('a, [`R]) Msg.t -> unit;
}

let get_unused_id t =
  match t.free_ids with
  | x :: xs ->
    t.free_ids <- xs;
    x
  | [] ->
    begin match t.role with
      | `Client -> assert (Int32.unsigned_compare t.next_id 0xFF000000l < 0);
      | `Server -> assert (Int32.unsigned_compare t.next_id 0xFF000000l >= 0);
    end;
    let x = t.next_id in
    t.next_id <- Int32.succ x;
    x

let id_allocated_by_us conn id =
  let is_service_id = Int32.unsigned_compare id 0xFF000000l >= 0 in
  match conn.role with
  | `Server -> is_service_id
  | `Client -> not is_service_id

(* Push [id] on to the stack of free IDs if we allocated it. *)
let free_id t id =
  if id_allocated_by_us t id then
    t.free_ids <- id :: t.free_ids

(* Call this when adding an item to an empty [outbox].
   On exit, the outbox is empty again. *)
let rec transmit t =
  let msg = Queue.peek t.outbox in  (* Just peek, to show that we're still running *)
  let buffer = Msg.buffer msg in
  (* todo: could do several messages at once *)
  Switch.run (fun sw ->
      let wrap fd = Eio_unix.Fd.of_unix ~sw ~close_unix:true fd in
      let unix_fds = Msg.fds msg in
      let fds = Queue.to_seq unix_fds |> Seq.map wrap |> List.of_seq in
      Queue.clear unix_fds;
      t.transport#send (Cstruct.of_bigarray buffer) fds
    );
  ignore (Queue.pop t.outbox);
  if not (Queue.is_empty t.outbox) then
    transmit t

let cancel_msg (m : (_, [`W]) Msg.t) =
  Queue.iter Unix.close (Msg.fds m)

let enqueue t msg =
  let start_transmit_thread = Queue.is_empty t.outbox in
  Queue.add msg t.outbox;
  if start_transmit_thread then (
    Fiber.fork ~sw:t.sw
      (fun () ->
         try
           Fun.protect (fun () -> transmit t)
             ~finally:(fun () -> Queue.iter cancel_msg t.outbox; Queue.clear t.outbox)
         with Eio.Io _ as ex when not t.transport#up ->
           Log.debug (fun f -> f "Transmit failed (but connection already shut down): %a" Fmt.exn ex);
      )
  )

let pp_proxy f (type a) (x: (a, _) proxy) =
  let (module M : Metadata.S with type t = a) = x.handler#metadata in
  Fmt.pf f "%s#%lu" M.interface x.id
