open Wayland_server

type t = {
  conn : [`Server] Connection.t;
  wl_display : [`V1] Wl_display.t;
}

module type TRACE = Proxy.TRACE with type role = [`Server]

module Trace : TRACE = struct
  type role = [`Server]

  let inbound (type a) (proxy : (a, _, _) Proxy.t) msg =
    Log.info (fun f ->
        let (module M : Metadata.S with type t = a) = Proxy.metadata proxy in
        let msg_name, arg_info = M.requests (Msg.op msg) in
        f "@[<h><- %a.%s %a@]"
          Proxy.pp proxy
          msg_name
          (Msg.pp_args arg_info) msg
      )

  let outbound (type a) (proxy : (a, _, _) Proxy.t) msg =
    Log.info (fun f ->
        let (module M) = Proxy.metadata proxy in
        let msg_name, arg_info = M.events (Msg.op msg) in
        f "@[<h>-> %a.%s %a@]"
                 Proxy.pp proxy
                 msg_name
                 (Msg.pp_args arg_info) msg
      )
end


let connect ?(trace=(module Trace : TRACE)) transport handler =
  let conn, wl_display = Connection.connect ~trace `Server transport handler in
  { conn; wl_display }

let wl_display t = t.wl_display

let closed t = Connection.closed t.conn
