type t
(** An XML node (the document node or an element). *)

val take_attr : string -> t -> string
(** [take_attr name t] removes the attribute [name] from [t]'s attributes and returns its value.
    It reports an error if the attribute does not exist. *)

val take_attr_opt : string -> t -> string option
(** [take_attr_opt name attrs] is like [take_attr], but returns [None] if the attribute is not present. *)

val take_elements : string -> t -> (t -> 'a) -> 'a list
(** [take_elements name t f] removes all the direct child elements named
    [name] from [t] and processes each one with [f].
    It is an error if [f] fails to remove all the attributes and child nodes
    from the node it processes. *)

val take_sole : string -> t -> (t -> 'a) -> 'a
(** [take_sole] is like [take_elements], but requires exactly one element to match. *)

val take_sole_opt : string -> t -> (t -> 'a) -> 'a option
(** [take_sole_opt] is like [take_elements], but requires at most one element to match. *)

val take_data : t -> string
(** [take_data t] removes the data content of [t] and returns it.
    It is an error if the node mixes text and child elements. *)

val parse : name:string -> in_channel -> (t -> 'a) -> 'a
(** [parse ~name ch f] reads an XML document from [ch] and transforms the document node with [f].
    It is an error if [f] fails to take the root element. *)

val pp : t Fmt.t
