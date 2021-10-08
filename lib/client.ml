open Wayland_client

let log_msg_src = Logs.Src.create "wayland-client" ~doc:"Wayland client messages"
module Log_msg = (val Logs.src_log log_msg_src : Logs.LOG)

let init_logging = lazy (
  match Sys.getenv_opt "WAYLAND_DEBUG" with
  | Some ("1" | "client") -> Logs.Src.set_level log_msg_src (Some Logs.Debug)
  | _ -> ()
)

type t = {
  conn : [`Client] Connection.t;
  wl_display : [`V1] Wl_display.t;
}

module type TRACE = Proxy.TRACE with type role = [`Client]

module Trace : TRACE = struct
  type role = [`Client]

  let inbound (type a) (proxy : (a, _, _) Proxy.t) msg =
    Log_msg.debug (fun f ->
        let (module M : Metadata.S with type t = a) = Proxy.metadata proxy in
        let msg_name, arg_info = M.events (Msg.op msg) in
        f "@[<h><- %a.%s %a@]"
          Proxy.pp proxy
          msg_name
          (Msg.pp_args arg_info) msg
      )

  let outbound (type a) (proxy : (a, _, _) Proxy.t) msg =
    Log_msg.debug (fun f ->
        let (module M) = Proxy.metadata proxy in
        let msg_name, arg_info = M.requests (Msg.op msg) in
        f "@[<h>-> %a.%s %a@]"
                 Proxy.pp proxy
                 msg_name
                 (Msg.pp_args arg_info) msg
      )
end

let connect ?(trace=(module Trace : TRACE)) transport =
  Lazy.force init_logging;
  let conn, wl_display = Connection.connect ~trace `Client transport @@ object
      inherit [_] Wl_display.v1

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
      inherit [_] Wl_callback.v1
      method on_done ~callback_data:_ = Lwt.wakeup set_result ()
    end
  in
  result

let wl_display t = t.wl_display

let set_paused t = Connection.set_paused t.conn
let dump f t = Connection.dump f t.conn
