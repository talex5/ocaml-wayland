open Lwt.Syntax

module Objects = Map.Make(Int32)

type connection = {
  socket : Lwt_unix.file_descr;
  role : [`Client | `Server];
  mutable objects : generic_handler Objects.t;
  mutable free_ids : int32 list;
  mutable next_id : int32;
  incoming_fds : Unix.file_descr Queue.t;
  outbox : (unit, [`W]) Msg.t Queue.t;   (* The transmit thread is running whenever this is non-empty. *)
} and 'a proxy = {
  id : int32;
  conn : connection;
  metadata : (module Metadata.S);
  version : int32;
  mutable valid : bool;
}
and generic_handler = Handler : 'a proxy * 'a handler -> generic_handler
and 'a handler = 'a proxy -> ('a, [`R]) Msg.t -> unit

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
  let fds = Msg.fds msg in
  let v = Lwt_unix.IO_vectors.create () in
  Lwt_unix.IO_vectors.append_bigarray v buffer 0 (Lwt_bytes.length buffer);  (* todo: could do several at once *)
  let rec send_all () =
    let fds = Queue.to_seq fds |> List.of_seq in
    let* sent = Lwt_unix.send_msg ~socket:t.socket ~io_vectors:v ~fds in
    List.iter Unix.close fds;
    Lwt_unix.IO_vectors.drop v sent;
    if Lwt_unix.IO_vectors.is_empty v then Lwt.return_unit
    else send_all ()
  in
  let* () = send_all () in
  ignore (Queue.pop t.outbox);
  if Queue.is_empty t.outbox then
    Lwt.return_unit
  else
    transmit t

let enqueue t buf =
  let start_transmit_thread = Queue.is_empty t.outbox in
  Queue.add buf t.outbox;
  if start_transmit_thread then Lwt.async (fun () -> transmit t)

let pp_proxy f (x: _ proxy) =
  let (module M) = x.metadata in
  Fmt.pf f "%s@%ld" M.interface x.id
