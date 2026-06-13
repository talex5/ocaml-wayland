(** {2 Types} *)

type ('a, +'v, 'role) t = ('a, 'v, 'role) Internal.versioned_proxy
(** An [('a, 'v, 'role) t] is a proxy used by ['role] to send messages to an object with interface ['a] and version in ['v]. *)

val user_data : ('a, _, 'role) t -> ('a, 'role) S.user_data
(** [user_data t] returns the data attached to the proxy when it was created.

    This just calls the application's [user_data] method, if any.
    Returns [No_data] if nothing was attached. *)

val cast_version : ('a, _, 'role) t -> ('a, _, 'role) t
(** If the version rules turn out to be too restrictive, this can be used to disable them.

    Using this incorrectly may lead to a protocol error (such as receiving an event for which
    no handler was registered). *)

(** {2 Metadata} *)

val version : _ t -> int32

val metadata : ('a, _, 'role) t -> (module Metadata.S with type t = 'a)

val ty : ('a, _, 'role) t -> 'a Metadata.ty

val interface : _ t -> string

(** {2 Lifecycle} *)

val delete : (_, _, [< `Client | `Server]) t -> unit
(** [delete t] removes [t] from the object table.
    For servers, it also sends a delete event from object 1 if the object was client-allocated. *)

val on_delete : (_, _, _) t -> (unit -> unit) -> unit
(** [on_delete t f] calls [f] when [t] is deleted, either by [delete] being called (on the server)
    or when the client receives confirmation of the deletion from the server.
    [f] will also be called if the connection ends. *)

val can_send : (_, _, _) t -> bool
(** [can_send t] is [true] if the proxy can still be used in out-going messages.
    It becomes false after calling {!delete} or {!shutdown_send}. *)

val transport_up : (_, _, _) t -> bool
(** [transport_up t] is [true] if [t]'s connection is still active.
    This is [false] after we read or send end-of-file on the connection. *)

(** {2 Handlers} *)

type ('a, 'v, 'role) proxy := ('a, 'v, 'role) t               (** Alias for use inside this file only *)

(** Handlers for normal objects. *)
module Handler : sig
  class type ['a, 'v, 'role] t = object
    method user_data : ('a, 'role) S.user_data
    method metadata : (module Metadata.S with type t = 'a)
    method dispatch : ('a, 'v, 'role) proxy -> ('a, [`R]) Msg.t -> unit
  end
  (** An [('a, 'v, 'role) t] handles incoming messages for an object of type ['a].

      Typically, a constructor will let the user pick from a range of versions
      for ['v], which will then be constrained by the [spawn] call.
      The generated bindings provide implementations of this where [dispatch] collects the
      arguments and passes them to an appopriately-typed virtual method,
      which must be implmented by the application. *)

  val attach : ('a, 'v, 'role) proxy -> ('a, 'v, 'role) #t -> unit
  (** [attach proxy t] sets [t] as the handler for [proxy].

      Any method argument that corresponds to a new object will arrive as a half-initialsed proxy
      (created by the generated bindings using {!accept_new}).
      This function must be called immediately (before switching threads or using the proxy)
      so that any future events addressed to this object can be handled. *)

  val cast_version : ('a, _, 'role) #t -> ('a, _, 'role) t
  (** If the version rules turn out to be too restrictive, this can be used to disable them.
      Using this incorrectly may lead to a protocol error (such as receiving an event for which
      no handler was registered). *)

  (** {2 Functions for use by generated code} *)

  val accept_new :
    (_, 'v, [< `Client | `Server ] as 'role) proxy ->
    (module Metadata.S with type t = 'a) ->
    int32 -> ('a, 'v, 'role) proxy
  (** [accept_new parent id] registers a new object, with an ID allocated by the peer.

      The generated bindings call this when receiving a new object,
      before passsing the resulting half-initialised proxy to the application code. *)
end

(** Handlers for services (objects added via the registry). *)
module Service_handler : sig
  class type ['a, 'v, 'role] t = object
    inherit ['a, 'v, 'role] Handler.t
    method min_version : int32
    method max_version : int32
  end
  (** An [('a, 'v, 'role) t] handles incoming messages for a service object of type ['a].

      This type is used when binding a service, to choose the version.
      All other handlers get the version from elsewhere (inherited from their parent
      object). *)

  val attach_proxy : ('a, [`Unknown], 'role) proxy -> ('a, ([> `V1] as 'v), 'role) #t -> ('a, 'v, 'role) proxy
  (** [attach_proxy p t] sets [t] as the handler for [p].

      Any method argument that corresponds to a new object will arrive as a half-initialsed proxy
      (created by the generated bindings using {!accept_new}).
      This function must be called immediately (before switching threads or using the proxy)
      so that any future events addressed to this object can be handled. *)

  val attach : ('a, [`Unknown], 'role) proxy -> ('a, [> `V1], 'role) #t -> unit
  (** Like [attach_proxy], but ignores the resulting proxy.
      Useful if the object only needs to respond to messages from the peer,
      but otherwise doesn't do anything. *)

  val interface : (_, _, _) #t -> string
  (** [interface t] is the interface from [t]'s metadata. *)

  val min_version : (_, _, _) #t -> int32
  (** [min_version t] is the minimum version supported by [t]. *)

  val cast_version : ('a, _, 'role) t -> ('a, _, 'role) t
  (** If the version rules turn out to be too restrictive, this can be used to disable them.
      Using this incorrectly may lead to a protocol error (such as receiving an event for which
      no handler was registered). *)

  (** {2 Functions for use by generated code} *)

  val accept_new :
    (_, 'v, [< `Client | `Server ] as 'role) proxy -> int32 ->
    (module Metadata.S with type t = 'a) ->
    version:int32 ->
    ('a, [`Unknown], 'role) proxy
  (** [accept_new parent id metadata ~version] registers a new object,
      with an ID allocated by the peer.

      The generated bindings call this when receiving a new object,
      before passsing the resulting half-initialised proxy to the application code. *)
end

(** {2 Functions for use by generated code}

    You should not need to use these functions directly.
    Instead, run wayland-scanner-ocaml to generate typed wrappers and use the wrappers instead. *)

val id : _ t -> int32
(** [id t] is [t]'s object ID. Use this to refer to the object in a message.
    @raise Invalid_argument if the proxy has been destroyed (can no longer be used for sending). *)

val id_opt : _ t option -> int32
(** Like [id] but returns 0l for [None]. *)

val alloc : ('a, _, _) t -> op:int -> ints:int -> strings:string option list -> arrays:string list -> ('a, [`W]) Msg.t
(** [alloc t ~op ~ints ~strings ~arrays] is a fresh message for [t]'s [op] operation.
    The message is the right size for [ints] integer arguments, all the strings in [strings],
    and all the arrays in [arrays]. *)

val send : ('a, _, [< `Client | `Server ]) t -> ('a, [`W]) Msg.t -> unit
(** [send t msg] enqueues [msg] on [t]'s connection. *)

val spawn : (_, 'v, [< `Client | `Server ] as 'role) t -> ('a, 'v, 'role) #Handler.t -> ('a, 'v, 'role) t
(** Create a new proxy on [t]'s connection with an unused ID.
    The new object has the same version as its parent. *)

val spawn_bind : (_, _, [< `Client | `Server ] as 'role) t -> (('a, 'v, 'role) #Service_handler.t * int32) -> ('a, 'v, 'role) t
(** Like [spawn] but the child's version is given explicitly, not inherited from the parent.
    This is used for binding with the global registry. *)

val shutdown_send : _ t -> unit
(** [shutdown_send t] indicates that you will no longer call [send] on [t].
    Call this after sending a destructor message.
    Attempts to send using the proxy after this will fail. *)

val shutdown_recv : _ t -> unit
(** [shutdown_recv t] indicates that we will no longer receive messages
    addressed to [t].
    Call this after receiving a destructor message. *)

val unknown_event : int -> string
(** A suitable string to display for an unknown event number. *)

val unknown_request : int -> string
(** A suitable string to display for an unknown request number. *)

(** {2 Logging and tracing} *)

(** Pass a [TRACE] module when connecting to trace protocol messages. *)
module type TRACE = sig
  type role
  val outbound : ('a, 'v, role) t -> ('a, [`W]) Msg.t -> unit
  val inbound : ('a, 'v, role) t -> ('a, [`R]) Msg.t -> unit
end

val pp : _ t Fmt.t

val pp_transport : _ t Fmt.t
(** [pp_transport] calls {!S.transport#pp} on the proxy's transport. *)

(**/**)

type ('v, 'role) generic = Proxy : ('a, 'v, 'role) t -> ('v, 'role) generic
(** A proxy whose type isn't known statically. Use {!ty} and pattern matching to recover the type. *)

val add_root : 'role Internal.connection -> ('a, 'v, 'role) #Handler.t -> ('a, 'v, 'role) t
(** [add_root conn h] sets [h] as the handler for object 1. *)

val delete_other : (_, _, [`Client]) t -> int32 -> unit
(** [delete_other proxy id] removes [id] from [proxy]'s connection. Internal use only. *)

val lookup_other : (_, _, 'role) t -> int32 -> (_, 'role) generic
(** [lookup_other parent id] returns the proxy with [id] in [parent]'s connection.
    Raises an exception if the object doesn't exist. *)

val wrong_type : parent:_ t -> expected:string -> _ t -> 'a
(** [wrong_type ~parent ~expected t] fails with an exception complaining that [t] should have type [expected]. *)

val trace : (module TRACE with type role = 'role) -> 'role Internal.tracer

val post_error : ('a, [>`V1], [<`Server]) t -> code:int32 -> message:string -> 'b
(** [post_error t ~code ~message] raises a protocol error with code [code] and message [message].
    It does not return. *)

exception Error of { object_id: int32; code: int32; message: string }
(** Fatal error event.

    Raised by servers to indicate a protocol error.

    Clients should not raise this exception.  If they do, it will be treated as any other
    uncaught exception. *)
