type ('a, 'rw) t constraint 'rw = [< `R | `W]
(** An [('a, 'rw) t] is a message for the ['a] interface.
    ['rw] is [`R] for an incoming message to be read, or
    [`W] for an outgoing message to be written. *)

val pp_args : Metadata.arg list -> _ t Fmt.t
(** [pp_args types] is a formatter for messages with argument types [types]. *)

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
    will be sent to the server. *)

val invalid_method : string -> 'a
(** Raise an error indicating that a message is invalid *)
