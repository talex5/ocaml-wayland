type ('a, 'rw) t constraint 'rw = [< `R | `W]
(** An [('a, 'rw) t] is a message for the ['a] interface.
    ['rw] is [`R] for an incoming message to be read, or
    [`W] for an outgoing message to be written. *)

val pp_args : Metadata.arg list -> _ t Fmt.t
(** [pp_args types] is a formatter for messages with argument types [types]. *)

(** {2 Protocol Constants} *)

val hdr_len : int
(** [hdr_len] is the length of the Wayland protocol header (8 bytes). *)

val max_msg_len : int
(** [max_msg_len] is the maximum length of an encoded Wayland message in bytes,
    including the 8-byte header.  It is equal to 4096 bytes. *)

val max_array_len : int
(** [max_array_len] is the maximum length a Wayland array.
    It is equal to [max_msg_len - hdr_len - 4], where the 4 is
    from the length prefix. *)

val max_string_len : int
(** [max_string_len] is the maximum length of a Wayland string in bytes.
    It is equal to [max_array_len - 1], where the 1 is for the ['\0']-terminator. *)
    
(** {2 Reading messages} *)

val parse : fds:Unix.file_descr Queue.t -> Cstruct.t -> ('a, [`R]) t option
(** [parse ~fds buf] returns the first message in [buf], if any.
    When [get_fd] is called, it will take one from [fds].
    Returns [None] if [buf] does not contain a complete message. *)

val obj : _ t -> int32
(** [obj t] is the object ID in the message header. *)

val op : _ t -> int
(** [op t] is the operation ID in the message header. *)

val length : (_, [`R]) t -> int
(** [length t] is the total size of the message (including any space for arguments not yet added). *)

val get_int    : _ t -> int32
(** [get_int t] returns the next argument (which must be an integer) from [t] and advances the next-argument pointer. *)

val get_string : _ t -> string
(** [get_string t] returns the next argument (which must be a string) from [t] and advances the next-argument pointer.
    The string returned does not include the trailing ['\0']. *)

val get_string_opt : _ t -> string option
(** [get_string_opt t] returns the next argument (which must be a string or NULL) from [t] and advances the next-argument pointer.
    The string returned does not include the trailing ['\0']. *)

val get_array  : _ t -> string
(** [get_array t] returns the next argument (which must be array) from [t] and advances the next-argument pointer. *)

val get_fd     : _ t -> Unix.file_descr
(** [get_fd t] takes the next FD from the queue. It raises an exception if the queue is empty.
    Note that the Wayland protocol does not indicate which FDs below to which messages,
    so if used incorrectly this will remove FDs intended for other messages.
    The caller takes ownership of the FD and is responsible for closing it. *)

val get_fixed  : _ t -> Fixed.t
(** [get_fixed t] returns the next argument (which must be a fixed-point number) from [t] and advances the next-argument pointer. *)

val check_end : _ t -> bool -> unit
(** [finished t] checks that the message is finished.

    On the receiving side, set the boolean argument to true, which raises a protocol error.
    On the sending side, set the boolean argument to false, which raises [Invalid_argument]. *)

(** {2 Generating messages} *)

val alloc : obj:int32 -> op:int -> ints:int -> strings:string option list -> arrays:string list -> ('a, [`W]) t
(** See {!Proxy.alloc}. *)

val add_int        : (_, [`W]) t -> int32 -> unit
val add_string     : (_, [`W]) t -> string -> unit
val add_string_opt : (_, [`W]) t -> string option -> unit
val add_array      : (_, [`W]) t -> string -> unit

val add_fd         : (_, [`W]) t -> Unix.file_descr -> unit
(** [add_fd t fd] adds a copy of [fd] (made with [Unix.dup]) to the message. *)

val add_fixed      : (_, [`W]) t -> Fixed.t -> unit

(**/**)

val buffer : (_, [`W]) t -> Cstruct.buffer
val fds : (_, [`W]) t -> Unix.file_descr Queue.t

val cast : ('a, 'rw) t -> ('b, 'rw) t

(** {3 Reporting errors} *)

exception Error of { object_id: int32; code: int32; message: string }
(** Exception raised by handlers to indicate a protocol violation.

    Servers must use this exception to raise protocol errors required
    by the protocol.  The object_id, code, and message are used to
    construct a message sent to the client before the client is
    disconnected.  Clients can raise this exception too, but no message
    will be sent to the server.

    Functions in this library that report protocol errors do so by raising
    this exception.  Until this exception is caught by ocaml-wayland,
    the connection stays open.  Therefore, one can catch this exception
    to prevent the client from being disconnected.
 *)

val invalid_method : string -> 'a
(** Raise an error indicating that a message is invalid *)

val bad_implementation : string -> 'a
(** Raise an error indicating that there is a bug in the compositor,
    but the bug is not so severe that the compositor must exit.
    The client that made the request currently being handled will
    be disconnected with a [Wayland_proto.Wl_display.Error.Implementation] error
    including the provided message.

    Other uses for this function include:

    - A protocol requires an error to be raised, but does not provide
      a suitable error.  This indicates a bug in the protocol specification.

    - The client's request is valid, but the compositor is unable to honor it.
      For instance, a compositor might use this error to indicate that a client
      exceeded an arbitrary limit. *)
