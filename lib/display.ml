open Lwt.Syntax
open Wayland_client

type t = {
  conn : Connection.t;
  wl_display : [`V1] Wl_display.t;
}

let connect () =
  let* conn, wl_display = Connection.connect @@ Wl_display.v1 @@ object
      method on_error _ ~object_id ~code ~message =
        Log.err (fun f -> f "Received Wayland error: %ld %s on object %ld" code message object_id)

      method on_delete_id proxy ~id =
        Proxy.delete proxy id
    end
  in
  Lwt.async (fun () -> Connection.listen conn);
  Lwt.return { conn; wl_display }

let sync t =
  let result, set_result = Lwt.wait () in
  let _ : _ Wl_callback.t = Wl_display.sync t.wl_display @@ Wl_callback.v1 @@ object
      method on_done _ ~callback_data:_ = Lwt.wakeup set_result ()
    end
  in
  result

let wl_display t = t.wl_display
