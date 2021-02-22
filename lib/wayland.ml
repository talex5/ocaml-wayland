module Unix_transport = Unix_transport
(** Send messages over a Unix-domain socket. *)

module Display = Display
(** Connect to a Wayland server and interact with the initial display object. *)

module Registry = Registry
(** A simple wrapper for the {!Wayland_client.Wl_registry} API. *)

(** [callback fn] is a simple wrapper for the {!Wayland_client.Wl_callback} API.
    It calls [fn data] when the callback's "done" signal is received.
    Wl_callback seems to be an exception to the usual Wayland versioning rules
    (a wl_callback can be created by multiple objects). *)
let callback fn : ([ `Wl_callback ], [> `V1]) Proxy.Handler.t =
  Proxy.Handler.cast_version @@ Wayland_client.Wl_callback.v1 @@ object
    method on_done _ ~callback_data = fn callback_data
  end

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
