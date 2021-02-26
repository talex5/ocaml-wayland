open Wayland_server

type t = {
  conn : [`Server] Connection.t;
  wl_display : [`V1] Wl_display.t;
}

let connect transport handler =
  let conn, wl_display = Connection.connect `Server transport handler in
  { conn; wl_display }

let wl_display t = t.wl_display

let closed t = Connection.closed t.conn
