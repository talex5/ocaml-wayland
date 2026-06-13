open Wayland_server

let log_msg_src = Logs.Src.create "wayland-server" ~doc:"Wayland server messages"
module Log_msg = (val Logs.src_log log_msg_src : Logs.LOG)

let init_logging = lazy (
  match Sys.getenv_opt "WAYLAND_DEBUG" with
  | Some ("1" | "server") -> Logs.Src.set_level log_msg_src (Some Logs.Debug)
  | _ -> ()
)

type t = {
  conn : [`Server] Connection.t;
  wl_display : [`V1] Wl_display.t;
}

module type TRACE = Proxy.TRACE with type role = [`Server]

module Trace : TRACE = struct
  type role = [`Server]

  let inbound (type a) (proxy : (a, _, _) Proxy.t) msg =
    Log.debug (fun f ->
        let (module M : Metadata.S with type t = a) = Proxy.metadata proxy in
        let msg_name, arg_info = M.requests (Msg.op msg) in
        f "@[<h><- %a.%s %a@]"
          Proxy.pp proxy
          msg_name
          (Msg.pp_args arg_info) msg
      )

  let outbound (type a) (proxy : (a, _, _) Proxy.t) msg =
    Log.debug (fun f ->
        let (module M) = Proxy.metadata proxy in
        let msg_name, arg_info = M.events (Msg.op msg) in
        f "@[<h>-> %a.%s %a@]"
                 Proxy.pp proxy
                 msg_name
                 (Msg.pp_args arg_info) msg
      )
end


let connect ?(trace=(module Trace : TRACE)) ~sw transport handler =
  Lazy.force init_logging;
  let conn, wl_display = Connection.connect ~sw ~trace `Server transport handler in
  { conn; wl_display }

let wl_display t = t.wl_display

let implementation_error t message =
  Connection.error t.conn ~object_id:1l ~code:3l ~message

let no_memory t message =
  Connection.error t.conn ~object_id:1l ~code:2l ~message

let dump f t = Connection.dump f t.conn
let stop t = Connection.stop t.conn
