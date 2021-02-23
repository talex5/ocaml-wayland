type ('a, +'v) t
(** An [('a, 'v) t] sends messages to an object with interface ['a] and version in ['v]. *)

type ('a, 'v) proxy := ('a, 'v) t               (* Alias for use inside this file only *)

type generic = Proxy : 'a Iface_reg.ty * ('a, [`Unknown]) t -> generic
(** A proxy with its type metadata.
    Pattern match on the type metadata to discover the proxy's type. *)

val delete : _ t -> unit
(** [delete t] sends a delete event from object 1 and removes [t] from the
    object table. This is only used in server code. *)

val user_data : ('a, _) t -> 'a S.user_data
(** [user_data t] returns the data attached to the proxy when it was created.
    Returns [No_data] if nothing was attached. *)

val cast_version : ('a, _) t -> ('a, _) t
(** If the version rules turn out to be too restrictive, this can be used to disable them.
    Using this incorrectly may lead to a protocol error (such as receiving an event for which
    no handler was registered). *)

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

  val accept_new : (_, 'v) proxy -> int32 -> ('a, 'v) proxy
  (** [accept_new parent id] registers a new object, with an ID allocated by the peer.
      The resulting proxy is in a half-initialised state.
      You must call {!attach} on it before switching threads or doing anything else with it. *)

  val attach : ('a, 'v) proxy -> ('a, 'v) t -> unit
  (** [attach proxy t] sets [t] as the handler for [proxy],
      which must be a partly initialised proxy returned by [accept_new]. *)
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

  val accept_new : (_, 'v) proxy -> int32 -> interface:Iface_reg.interface -> version:int32 -> generic
  (** [accept_new parent id ~interface ~version] registers a new object,
      with an ID allocated by the peer.
      The returned proxy must have its handlers attached before switching threads,
      since otherwise processing a message addressed to the new object will fail.
      This is called from the generated code; the user code then calls the result. *)

  val attach : ('a, [`Unknown]) proxy -> ('a, 'v) t -> ('a, 'v) proxy
  (** [attach proxy t] sets [t] as the handler for [proxy],
      which must be a partly initialised proxy returned by [accept_new].
      It returns the proxy with its version cast to the handler's version. *)
end

val id : _ t -> int32
(** [id t] is [t]'s object ID. Use this to refer to the object in a message. *)

val alloc : ('a, _) t -> op:int -> ints:int -> strings:string list -> arrays:string list -> ('a, [`W]) Msg.t
(** [alloc t ~op ~ints ~strings ~arrays] is a fresh message for [t]'s [op] operation.
    The message is the right size for [ints] integer arguments, all the strings in [strings],
    and all the arrays in [arrays]. *)

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

val lookup_other_unsafe : interface:string -> (_, 'v) t -> int32 -> (_, 'v) t
(** [lookup_other ~interface parent id] returns the proxy with [id] in [parent]'s connection.
    It assumes the child proxy has the same version.
    You MUST cast the result to the type [`Interface] (the equivalent of [interface]).
    @param interface Used to check that the object has the expected type, since the other end
                     could send us an invalid ID. *)
