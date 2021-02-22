type ('a, +'v) t
(** An [('a, 'v) t] sends messages to an object with interface ['a] and version in ['v]. *)

type ('a, 'v) proxy := ('a, 'v) t               (* Alias for use inside this file only *)

val user_data : ('a, _) t -> 'a S.user_data
(** [user_data t] returns the data attached to the proxy when it was created.
    Returns [No_data] if nothing was attached. *)

(** {2 Functions for use by generated code}

    You should not need to use these functions directly.
    Instead, run wayland-scanner-ocaml to generate typed wrappers and use the wrappers instead. *)

module Handler : sig
  type ('a, 'v) t
  (** An [('a, 'v) t] handles incoming messages for an object of type ['a].
      Typically, a constructor will let the user pick from a range of versions
      for ['v], which will then be constrained by the [spawn] call. *)

  val v :
    ?user_data:'a S.user_data ->
    (module Metadata.S) ->
    (('a, 'v) proxy -> ('a, [`R]) Msg.t -> unit) ->
    ('a, 'v) t
    (** [v metadata dispatch] is a handler for the interface [metadata],
        which uses [dispatch self msg] to handle incoming messages.
        Only used by the generated code.
        @param user_data Extra data to be attached to the proxy. *)

  val cast_version : ('a, _) t -> ('a, _) t
  (** If the version rules turn out to be too restrictive, this can be used to disable them.
      Using this incorrectly may lead to a protocol error (such as receiving an event for which
      no handler was registered). *)
end

module Service_handler : sig
  type ('a, 'v) t
  (** An [('a, 'v) t] handles incoming messages for a service object of type ['a].
      The difference between services and other objects is that a service gets its version at runtime
      from the bind request, whereas other objects inherit their version from their parent. *)

  val v :
    version:int32 ->
    ?user_data:'a S.user_data ->
    (module Metadata.S) ->
    (('a, 'v) proxy -> ('a, [`R]) Msg.t -> unit) ->
    ('a, 'v) t
    (** [v ~version metadata dispatch] is a handler for the interface [metadata],
        which uses [dispatch self msg] to handle incoming messages.
        Only used by the generated code.
        @param version The version to request in the bind call.
        @param user_data Extra data to be attached to the proxy. *)

  val interface : _ t -> string
  (** [interface t] is the interface from [t]'s metadata. *)

  val version : _ t -> int32
  (** [version t] is the version from [t]. *)

  val cast_version : ('a, _) t -> ('a, _) t
  (** If the version rules turn out to be too restrictive, this can be used to disable them.
      Using this incorrectly may lead to a protocol error (such as receiving an event for which
      no handler was registered). *)
end

val id : _ t -> int32
(** [id t] is [t]'s object ID. Use this to refer to the object in a message. *)

val alloc : ('a, _) t -> op:int -> ints:int -> strings:string list -> ('a, [`W]) Msg.t
(** [alloc t ~op ~ints ~strings] is a fresh message for [t]'s [op] operation.
    The message is the right size for [ints] integer arguments and all the strings in [strings]. *)

val send : ('a, _) t -> ('a, [`W]) Msg.t -> unit
(** [send t msg] enqueues [msg] on [t]'s connection. *)

val spawn : (_, 'v) t -> ('a, 'v) Handler.t -> ('a, 'v) t
(** Create a new proxy on [t]'s connection with an unused ID.
    The new object has the same version as its parent. *)

val spawn_bind : _ t -> ('a, 'v) Service_handler.t -> ('a, 'v) t
(** Like [spawn] but the child's version is taken from the handler,
    not inherited from the parent.
    This is used for binding with the global registry. *)

val invalidate : _ t -> unit
(** [invalidate t] flags the proxy as no longer valid.
    Call this after calling a destructor method to prevent accidentally
    trying to use it again.
    Attempts to use the proxy after this will fail. *)

val unknown_event : int -> string
(** A suitable string to display for an unknown event number. *)

val unknown_request : int -> string
(** A suitable string to display for an unknown request number. *)

val pp : _ t Fmt.t

(**/**)

val add_root : Internal.connection -> ('a, 'v) Service_handler.t -> ('a, 'v) t
(** [add_root conn h] sets [h] as the handler for object 1. *)

val delete_other : _ t -> int32 -> unit
(** [delete_other proxy id] removes [id] from [proxy]'s connection. Internal use only. *)
