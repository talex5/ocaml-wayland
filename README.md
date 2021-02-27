# Pure OCaml Wayland protocol library

Status: **prototyping**

* [API documentation][]

Wayland is a communications protocol intended for use between processes on a single computer.
It is mainly used by graphical applications (clients) to talk to display servers,
but nothing about the protocol is specific to graphics and it could be used for other things.

A client connects to a server over a Unix-domain socket (e.g. by opening `/run/user/1000/wayland-0`).
Conceptually, the server hosts a number of objects (initially, a single object with ID 1).
The client can send "requests" *to* objects, and the server can send "events" *from* objects.
On the wire, a message is an object ID, an operation ID (request or event), and a list of arguments.

The protocol is described in more detail in [The Wayland Protocol][] (which is however only half written, as of Feb 2021).

## Features

- **File descriptor passing**: You can use any Unix file descriptor as a message argument and
  it will be sent over the Unix-domain socket using `SCM_RIGHTS`.
  For example, to send a frame of video you can get a file descriptor to some shared memory,
  draw the image to it, and then pass the FD to the server for rendering, with no need to copy the data.

- **Asynchronous**: Multiple messages can be sent in a row by either side. They will be processed in order.

- **Schema files**: Protocols are defined using a (fairly simple) XML format.
  Then a language-specific tool (e.g. `wayland-scanner-ocaml` from this package) generates typed bindings for it.

- **Extensible**: Multiple services can be added to a registry object and queried at runtime.

- **Versioning**: The schema file indicates which operations were added in each version.

## Limitations

- **Local only**: It is only possible to attach file descriptors to Unix-domain sockets,
  so the system is not very useful over TCP (unless you avoid using FDs completely).
  Also, there is no support for access-control (beyond Unix file permissions on the socket), encryption, etc.

- **Limited type system**: Argument types are limited to 32-bit integers (signed and unsigned),
  24.8 fixed point, strings, byte-arrays, object IDs, and file descriptors.
  If you want to e.g. send a list of strings then you would create a list object, then add each item to it,
  then pass the list object to the target, then destroy the list.
  However, this can all be done in one go and is fairly efficient.

- **Middleware is tricky**: If you want to write a proxy process that sits between a client and a server,
  it will need to understand the schema (including any extensions).
  This is because the framing does not say which FDs go with which messages.

If you are looking for an RPC protocol for use over the Internet, consider using [Cap'n Proto RPC][] instead.

## Getting started

`example/test.ml` contains a simple client, based on the example in "The Wayland Protocol" book.
To run it:

    dune exec -- ./example/test.exe

It shows a scrolling grid of squares.

See the [API documentation][] for more information.

## Interface versions

Each proxy object has phantom types for the interface and the version(s) of that interface.
e.g. a value of type ``([`Wl_surface], [`V3]) Proxy.t`` is a proxy to a version 3 surface object.

Functions that send messages require a compatible version.
For example, `Wl_surface.set_buffer_transform` was introduced in version 2,
and so it can be used with objects supporting version 2, 3 or 4:

```ocaml
module Wl_surface : sig
  ...
  val set_buffer_transform : [< `V2 | `V3 | `V4 ] t ->
    transform:Wayland_proto.Wl_output.Transform.t -> unit
  ...
end
```

In Wayland, there are two ways of introducing new objects to a conversation:

- In most cases, the schema gives the interface for the created object.
  In this case, the new object will have the same version as the object
  from which it was created.

- The registry's `bind` operation does not specify the interface.
  Instead, the client passes the interface and version as arguments.

When creating an object, you must give handlers for each message that could be received related to that object.
For example, when using the compositor object to create a surface you must supply a compatible set of handlers:

```ocaml
module Wl_compositor : sig
  ...
  val create_surface : [< `V1 | `V2 | `V3 | `V4 ] as 'v t ->
    ([ `Wl_surface ], 'v) Proxy.Handler.t ->
    ([ `Wl_surface ], 'v) Proxy.t
  ...
end
```

This says that `create_surface` can be used with any version `'v` of the compositor,
but you must supply handlers for version `'v` of the surface object.
The return value will be a surface proxy with the same version as the compositor.

There is one handler constructor function for each distinct version of an interface.
For example, to create a handler for version 3 of the surface interface you can use this function:

```ocaml
module Wl_surface : sig
  ...
  val v3 : ([< `V1 | `V2 | `V3 ] as 'v) h3 ->
           ([ `Wl_surface ], 'v) Proxy.Handler.t
  ...
end
```

The type for `v3` says that if you supply handlers for version 3 of the protocol then
you get a handler that can be used for object versions 1, 2 or 3.
When you try to create the surface using a compositor, the creation method will further
constrain the version to match the compositor's version.
So the ``[< `V1 | `V2 | `V3 ]`` constraint just prevents you from using this with e.g. a
v4 compositor.

For example, to use this with a ``[`V2 | `V3] Wl_compositor.t``
(that is, a compositor that is either v2 or v3), you would use `v3` at type:

```ocaml
val v3 : [`V2 | `V3] h3 -> ([ `Wl_surface ], [`V2 | `V3]) Proxy.Handler.t
```

The handlers you pass (`h3`) must handle any incoming v3 message,
but when sending they can only use messages available in both v2 and v3.

For top-level services (objects that are listed in the registry and for which the
client specifies the version explicitly), the constructor gives a
`Proxy.Service_handler.t` instead:

```ocaml
module Wl_compositor : sig
  ...
  val v3 : unit -> ([ `Wl_compositor ], [> `V3 ]) Proxy.Service_handler.t
  ...
end
```

When using `v3`, you don't have to worry about it being a v1 or v2 object,
as you will get the exact version requested.
Note: `Wl_compositor` doesn't produce any events, so the handlers are just `()` in this case.

The version types should prevent you from sending messages the other side won't understand,
or failing to handle a message the other side sends. However, they may be inflexible.
You can use `Handler.cast_version` to escape from the rules and manage things yourself if necessary.

## Attaching extra data to objects

Sometimes you may pass an object to the other process and then receive it back again later via another message.
This is mostly needed when implementing a server.
For example, consider a client that does the following:

1. Create a `Wl_region`.
2. Add some rectangles to the region.
3. Pass the region to `Wl_surface.set_input_region`.

When the server gets the region as an argument to `set_input_region`
it will want to recognise it as a region it created earlier,
with private state holding the list of rectangles.

You can attach data to a handler by specifying `~user_data` when creating the handler,
and get it back using `Proxy.user_data`.

Ideally, the application would define the `user_data` type,
but this would require functorising the whole API and all the bindings over the type, which is inconvenient.
Instead, `Wayland.S.user_data` is defined as an open variant type and you should extend that with a single entry for your (GADT) type. e.g.

```ocaml
type 'a my_user_data = 
  | Region : region_data -> [`Wl_region] my_user_data
  | Output : output_data -> [`Wl_output] my_user_data

type 'a Wayland.S.user_data += My_user_data of 'a my_user_data

let user_data (proxy : ('a, _) Proxy.t) : 'a my_user_data =
  match Wayland.Proxy.user_data proxy with
  | My_user_data x -> x
  | S.No_data -> Fmt.failwith "No data attached to %a!" Proxy.pp proxy
  | _ -> Fmt.failwith "Unexpected data attached to %a!" Proxy.pp proxy
```

Then when you receive a region proxy, do:

```ocaml
method on_set_input_region _ ~region =
  let Region r = user_data region in
  ...
```

Because the compiler knows that `region` is of type `Wl_region`,
it knows that the user data will be of type ``[`Wl_region] my_user_data``,
and so there is only case you have to handle.
To avoid the exceptions, you just need to ensure that:

1. You don't extend `Wayland.S.user_data` with any other variants.
2. You don't forget to attach the data when creating the handler.

## Resource lifetimes

The Wayland spec is a bit vague about object lifecycles.
This is my guess about how it's supposed to work.

Some message types are marked as "destructors".
Usually the client sends a destructor request and the server acknowledges it.
In some cases (e.g. callbacks, which have no requests), the server calls the destructor.

After sending a destructor message, you cannot reference that object in any further messages.
Except that the server can call a destructor and then also send a delete event mentioning the same object.

The client cannot reuse an ID until the server has confirmed the deletion,
since otherwise it wouldn't know whether a new message was for the old or new object.

Only servers can confirm object deletion though,
so if the server calls a destructor then there has to be some interface-specific method
for the client to call to confirm the deletion.
Also, servers only send delete confirmations for IDs the client allocated.

Examples:

1. The client creates a surface. Then it calls `destroy`. The client must no
   longer send messages to the surface, but continues to receive events about
   it. When the service sends `delete`, the client can reuse the ID.

2. The client creates a callback. The service calls `done` on it (a destructor),
   and then immediately sends a `delete` for it.
   There's no race here because the callback doesn't accept any messages from the client.

3. The server creates data offer. Then it creates another one.
   This second one implicitly destroys the original.
   The server must no longer send messages from the original offer, but may receive requests
   to it from the client.
   The client responds by calling the `release` method on the old selection.
   The server does not send any confirmation for that;
   the `release` is itself the confirmation of the implicit deletion from the `selection` event.

Unlike other handlers, client destructor handlers do not take a proxy argument
since the proxy is unusable by this point.

On the server side, the handler will normally respond to a destructor call
by calling `Proxy.delete` immediately.
However, when relaying to an upstream service it may be useful to delay this
until the upstream service has confirmed the deletion too.

## TODO

- Using `$WAYLAND_SOCKET` to pass an FD doesn't work yet.
- Version typing for server-side top-level objects needs work.

[The Wayland Protocol]: https://wayland-book.com/
[Cap'n Proto RPC]: https://github.com/mirage/capnp-rpc
[API documentation]: https://talex5.github.io/ocaml-wayland/wayland/index.html
