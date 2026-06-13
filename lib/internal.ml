open Eio.Std

module Objects = Map.Make(Int32)

type 'a role =
  | Server : [`Server] role
  | Client : [`Client] role

type +'a error =
  | Error : { id: int32; code: int32; message: string } -> 'a error
  | No_error : 'a error
  | Shutdown : 'a error
  | Done : 'a error

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
  mutable display_proxy : ([`Wl_display], [`V1], 'role) versioned_proxy option;
  mutable error : 'role error;
  promise : unit Eio.Promise.t;
  resolver : unit Eio.Promise.u;
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
} and ('a, +'v, 'role) versioned_proxy = ('a, 'role) proxy

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

let check_error t =
  match t.error with
  | No_error -> ()
  | Error { id; code;  message } ->
    t.error <- Done;
    let _msg: ([`Wl_display], [`W]) Msg.t = Msg.alloc ~obj:1l ~op:0 ~ints:3 ~strings:[(Some message)] ~arrays:[] in
    Msg.add_int _msg id;
    Msg.add_int _msg code;
    Msg.add_string _msg message;
    begin match t.display_proxy with
    | None -> ()
    | Some proxy -> t.trace.outbound proxy _msg
    end;
    t.transport#send (Cstruct.of_bigarray (Msg.buffer _msg)) [];
    t.transport#shutdown;
    Eio.Promise.resolve t.resolver ()
  | Shutdown ->
    t.error <- Done;
    t.transport#shutdown;
    Eio.Promise.resolve t.resolver ()
  | Done -> ()

(* Call this when adding an item to an empty [outbox].
   On exit, the outbox is empty again. *)
let rec transmit: [<`Client|`Server] connection -> unit = fun t ->
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
  if Queue.length t.outbox > 1 then (
    ignore (Queue.pop t.outbox);
    transmit t
  ) else (
    (* Transmit the error *then* pop the queue, so that we cannot yield
       while the queue is empty *)
    check_error t;
    ignore (Queue.pop t.outbox)
  )

let cancel_msg (m : (_, [`W]) Msg.t) =
  Queue.iter Unix.close (Msg.fds m)

let error (t: [<`Client|`Server] connection) ~id ~code ~message =
  match t.error with
  | No_error ->
    t.error <- Error { id; code; message };
    if Queue.is_empty t.outbox then
      Fiber.fork ~sw:t.sw
        (fun () ->
          try
            check_error t
          with Eio.Io _ as ex when not t.transport#up ->
            Log.debug (fun f -> f "Transmit failed (but connection already shut down): %a" Fmt.exn ex))
  | Error _ | Shutdown | Done -> ()

let shutdown (t: [<`Client|`Server] connection) =
  match t.error with
  | No_error ->
    t.error <- Shutdown;
    begin match Queue.length t.outbox with
    | 0 -> check_error t
    | _ -> Eio.Promise.await t.promise
    end
  | Error _ | Shutdown | Done -> Eio.Promise.await t.promise

let enqueue (t: 'a connection) msg =
  if t.error = No_error then
    let start_transmit_thread = Queue.is_empty t.outbox in
    Queue.add msg t.outbox;
    if start_transmit_thread then
      Fiber.fork ~sw:t.sw
        (fun () ->
          try
            Fun.protect (fun () -> transmit t)
              ~finally:(fun () -> Queue.iter cancel_msg t.outbox; Queue.clear t.outbox)
          with Eio.Io _ as ex when not t.transport#up ->
            Log.debug (fun f -> f "Transmit failed (but connection already shut down): %a" Fmt.exn ex))

let pp_proxy f (type a) (x: (a, _) proxy) =
  let (module M : Metadata.S with type t = a) = x.handler#metadata in
  Fmt.pf f "%s#%lu" M.interface x.id
