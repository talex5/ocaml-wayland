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

There is one constructor function for each distinct version of an interface.
e.g. `Wl_surface.v3` could be used to create the proxy above.

## TODO

- Only the client-side is working, although the server-side should be fairly easy.
- Using `$WAYLAND_SOCKET` to pass an FD doesn't work yet.
- Needs more testing.

[The Wayland Protocol]: https://wayland-book.com/
[Cap'n Proto RPC]: https://github.com/mirage/capnp-rpc
[API documentation]: https://talex5.github.io/ocaml-wayland/wayland/index.html
