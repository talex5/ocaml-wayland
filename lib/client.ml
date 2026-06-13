open Eio.Std
open Wayland_client

let log_msg_src = Logs.Src.create "wayland-client" ~doc:"Wayland client messages"
module Log_msg = (val Logs.src_log log_msg_src : Logs.LOG)

type error_callback = id:int32 -> code:int32 -> message:string -> unit

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

let connect ?(trace=(module Trace : TRACE)) ~sw ?error_callback:error_callback transport =
  Lazy.force init_logging;
  let conn, wl_display = Connection.connect ~sw ~trace `Client transport @@ object
      inherit [_] Wl_display.v1

      method on_error _ ~id ~code ~message =
        Fun.protect ~finally:(fun () -> transport#shutdown) @@ fun () ->
          match error_callback with
          | None -> Log.err (fun f -> f "Received Wayland error: %ld %S on object %ld" code message id)
          | Some cb -> cb ~id ~code ~message

      method on_delete_id proxy ~id =
        Proxy.delete_other proxy id
    end
  in
  { conn; wl_display }

let sync t =
  let result, set_result = Promise.create () in
  let _ : _ Wl_callback.t = Wl_display.sync t.wl_display @@ object
      inherit [_] Wl_callback.v1
      method on_done ~callback_data:_ = Promise.resolve set_result ()
    end
  in
  Promise.await result

let wl_display t = t.wl_display

let dump f t = Connection.dump f t.conn
let stop t = Connection.stop t.conn
