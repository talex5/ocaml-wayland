module Unix_transport = Unix_transport
(** Send messages over a Unix-domain socket. *)

module Client = Client
(** Connect to a Wayland server and interact with the initial display object. *)

module Registry = Registry
(** A simple wrapper for the {!Wayland_client.Wl_registry} API. *)

(** [callback fn] is a simple wrapper for the {!Wayland_client.Wl_callback} API.
    It calls [fn data] when the callback's "done" signal is received.
    Wl_callback seems to be an exception to the usual Wayland versioning rules
    (a wl_callback can be created by multiple objects). *)
let callback fn = object
  inherit [_] Wayland_client.Wl_callback.handlers
  method on_done ~callback_data = fn callback_data
end

module Server = Server
(** Code for writing Wayland servers. *)

module Fixed = Fixed
(** Wayland's 24.8 fixed-point type. *)

let src = Log.src
(** Control the library's log level. *)

(** {2 Generated bindings for the core Wayland protocol} *)

module Wayland_client = Wayland_client
(** Bindings for use by Wayland clients. *)

module Wayland_server = Wayland_server
(** Bindings for use by Wayland servers. *)

module Wayland_proto = Wayland_proto
(** Type metadata for use by bindings. *)

(** {2 Low-level API} *)

module Proxy = Proxy
(** Low-level operations on objects, mostly used by the generated wrappers. *)

module Metadata = Metadata
(** Describing interfaces. *)

module Msg = Msg
(** Building and parsing messages. *)

module S = S
(** Type signatures. *)

module Iface_reg = Iface_reg
(** Registry of known interfaces. Used by the generated code. *)
