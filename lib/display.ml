open Wayland_client

type t = {
  conn : [`Client] Connection.t;
  wl_display : [`V1] Wl_display.t;
}

module type TRACE = Proxy.TRACE with type role = [`Client]

module Trace : TRACE = struct
  type role = [`Client]

  let inbound (type a) (proxy : (a, _, _) Proxy.t) msg =
    Log.info (fun f ->
        let (module M : Metadata.S with type t = a) = Proxy.metadata proxy in
        let msg_name, arg_info = M.events (Msg.op msg) in
        f "@[<h><- %a.%s %a@]"
          Proxy.pp proxy
          msg_name
          (Msg.pp_args arg_info) msg
      )

  let outbound (type a) (proxy : (a, _, _) Proxy.t) msg =
    Log.info (fun f ->
        let (module M) = Proxy.metadata proxy in
        let msg_name, arg_info = M.requests (Msg.op msg) in
        f "@[<h>-> %a.%s %a@]"
                 Proxy.pp proxy
                 msg_name
                 (Msg.pp_args arg_info) msg
      )
end

let connect ?(trace=(module Trace : TRACE)) transport =
  let conn, wl_display = Connection.connect ~trace `Client transport @@ object
      inherit [_] Wl_display.handlers ~version:1l

      method on_error _ ~object_id ~code ~message =
        Log.err (fun f -> f "Received Wayland error: %ld %S on object %ld" code message object_id)

      method on_delete_id proxy ~id =
        Proxy.delete_other proxy id
    end
  in
  { conn; wl_display }, Connection.closed conn

let sync t =
  let result, set_result = Lwt.wait () in
  let _ : _ Wl_callback.t = Wl_display.sync t.wl_display @@ object
      inherit [_] Wl_callback.handlers
      method on_done ~callback_data:_ = Lwt.wakeup set_result ()
    end
  in
  result

let wl_display t = t.wl_display
