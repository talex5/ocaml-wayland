open Wayland_server

type t = {
  conn : [`Server] Connection.t;
  wl_display : [`V1] Wl_display.t;
}

let connect transport registry =
  let serial = ref 0l in
  let next_serial () =
    serial := Int32.succ !serial;
    !serial
  in
  let conn, wl_display = Connection.connect `Server transport @@ Wl_display.v1 @@ object
      method on_get_registry _ reg = registry reg

      method on_sync _ cb =
        Proxy.Handler.attach cb @@ Wl_callback.v1 ();
        Wl_callback.done_ cb ~callback_data:(next_serial ());
        Proxy.delete cb
    end
  in
  { conn; wl_display }

let wl_display t = t.wl_display

let closed t = Connection.closed t.conn
