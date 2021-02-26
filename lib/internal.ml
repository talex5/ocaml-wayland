open Lwt.Syntax

module Objects = Map.Make(Int32)

type 'role connection = {
  transport : S.transport;
  role : 'role;
  mutable objects : 'role generic_proxy Objects.t;
  mutable free_ids : int32 list;
  mutable next_id : int32;
  incoming_fds : Unix.file_descr Queue.t;
  outbox : (unit, [`W]) Msg.t Queue.t;          (* The transmit thread is running whenever this is non-empty. *)
  closed : (unit, exn) Lwt_result.t;
  set_closed : (unit, exn) result Lwt.u;
} and ('a, 'role) proxy = {
  id : int32;
  conn : 'role connection;
  version : int32;
  on_delete : (unit -> unit) Queue.t;
  mutable handler : ('a, 'role) handler;
  mutable valid : bool;
}
and 'role generic_proxy = Generic : ('a, 'role) proxy -> 'role generic_proxy
and ('a, 'role) handler = {
  user_data : ('a, 'role) S.user_data;
  metadata : (module Metadata.S with type t = 'a);
  dispatch : ('a, 'role) proxy -> ('a, [`R]) Msg.t -> unit;
}

let get_unused_id t =
  match t.free_ids with
  | x :: xs ->
    t.free_ids <- xs;
    x
  | [] ->
    assert (Int32.unsigned_compare t.next_id 0xFF000000l < 0);
    let x = t.next_id in
    t.next_id <- Int32.succ x;
    x

(* Push [id] on to the stack of free IDs. *)
let free_id t id =
  t.free_ids <- id :: t.free_ids

(* Call this when adding an item to an empty [outbox].
   On exit, the outbox is empty again. *)
let rec transmit t =
  let msg = Queue.peek t.outbox in  (* Just peek, to show that we're still running *)
  let buffer = Msg.buffer msg in
  let fds = Msg.fds msg |> Queue.to_seq |> List.of_seq in
  (* todo: could do several messages at once *)
  let* () = t.transport#send (Cstruct.of_bigarray buffer) fds in
  List.iter Unix.close fds;
  ignore (Queue.pop t.outbox);
  if Queue.is_empty t.outbox then
    Lwt.return_unit
  else
    transmit t

let cancel_msg (m : (_, [`W]) Msg.t) =
  Queue.iter Unix.close (Msg.fds m)

let enqueue t msg =
  if Lwt.is_sleeping t.closed then (
    let start_transmit_thread = Queue.is_empty t.outbox in
    Queue.add msg t.outbox;
    if start_transmit_thread then (
      Lwt.dont_wait
        (fun () -> transmit t)
        (fun ex ->
           if Lwt.is_sleeping t.closed then (
             Lwt.wakeup_exn t.set_closed ex;
           ) else
             Log.debug (fun f -> f "Transmit failed (but connection already closed): %a" Fmt.exn ex);
           Queue.iter cancel_msg t.outbox
        )
    )
  ) else (
    cancel_msg msg;
    failwith "Connection has been closed"
  )

let pp_proxy f (type a) (x: (a, _) proxy) =
  let (module M : Metadata.S with type t = a) = x.handler.metadata in
  Fmt.pf f "%s@%ld" M.interface x.id
