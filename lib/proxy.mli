type ('a, 'v) t
(** An [('a, 'v) t] sends messages to an object with interface ['a] and version in ['v]. *)

type ('a, 'v, 'vspawn, 'vbind) handler
(** An [('a, 'v, 'vspawn, 'vbind) handler] handles incoming messages for an object of type ['a].
    When created by a normal object (with [spawn]), it handles interface version ['v = 'vspawn],
    and when created using the registry (using [spawn_bind]) it handles version ['v = 'vbind].
    Typically, a constructor will let the user pick from a range of versions
    for ['vspawn], which will then be constrained by the [spawn] call. *)

val interface : _ t -> string
(** [interface t] is the interface from [t]'s handler metadata. *)

val version : _ t -> int32
(** [version t] is the actual interface version being used.
    Normally, this is inherited from the object that created this one, but for registry binds
    the version comes from the handler. *)

val cast_version : ('a, _, _, _) handler -> ('a, _, _, _) handler
(** If the version rules turn out to be too restrictive, this can be used to disable them.
    Using this incorrectly may lead to a protocol error (such as receiving an event for which
    no handler was registered). *)

(** {2 Functions for use by generated code}

    You should not need to use these functions directly.
    Instead, run wayland-scanner-ocaml to generate typed wrappers and use the wrappers instead. *)

val handler : (module Metadata.S) -> version:int32 ->
  (('a, 'v) t -> ('a, [`R]) Msg.t -> unit) ->
  ('a, 'v, _, _) handler
(** [handler metadata ~version dispatch] is a handler for the interface [metadata],
    which uses [dispatch self msg] to handle incoming messages.
    If used with [spawn_bind], [version] will be the requested version. *)

val id : _ t -> int32
(** [id t] is [t]'s object ID. Use this to refer to the object in a message. *)

val alloc : ('a, _) t -> op:int -> ints:int -> strings:string list -> ('a, [`W]) Msg.t
(** [alloc t ~op ~ints ~strings] is a fresh message for [t]'s [op] operation.
    The message is the right size for [ints] integer arguments and all the strings in [strings]. *)

val send : ('a, _) t -> ('a, [`W]) Msg.t -> unit
(** [send t msg] enqueues [msg] on [t]'s connection. *)

val spawn : (_, 'v) t -> ('a, 'v, 'v, _) handler -> ('a, 'v) t
(** Create a new proxy on [t]'s connection with an unused ID.
    The new object has the same version as its parent. *)

val spawn_bind : _ t -> ('a, 'v, _, 'v) handler -> ('a, 'v) t
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

val handler_interface : _ handler -> string
(** [handler_interface h] is the interface from [h]'s metadata.
    Used for bind-type calls. *)

val handler_version : _ handler -> int32
(** [handler_version h] is the version from [h].
    Used for bind-type calls. *)

val pp : _ t Fmt.t

(**/**)

val add_root : Internal.connection -> ('a, 'v, _, 'v) handler -> ('a, 'v) t
(** [add_root conn h] sets [h] as the handler for object 1. *)

val delete : _ t -> int32 -> unit
(** [delete proxy id] removes [id] from [proxy]'s connection. Internal use only. *)
