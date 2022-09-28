(* Example Wayland client, based on the C example from https://wayland-book.com/ *)

open Lwt.Syntax

open Wayland.Wayland_client
open Wayland_protocols.Xdg_shell_client

module Registry = Wayland.Registry

type t = {
  shm : [`V1] Wl_shm.t;
  surface : [`V4] Wl_surface.t;
  mutable fg : int32;
  mutable width : int;
  mutable height : int;
  mutable scroll : int;
  mutable vy : int;
}

(* Draw the content to [t.surface]. *)
let draw_frame t =
  let stride = t.width * 4 in
  let size = t.height * stride in
  let pool, data = Shm.with_memory_fd ~size (fun fd ->
      let pool = Wl_shm.create_pool t.shm (new Wl_shm_pool.v1) ~fd ~size:(Int32.of_int size) in
      let ba = Unix.map_file fd Bigarray.Int32 Bigarray.c_layout true [| t.height; t.width |] in
      pool, Bigarray.array2_of_genarray ba
    ) in
  let buffer =
    Wl_shm_pool.create_buffer pool
      ~offset:0l
      ~width:(Int32.of_int t.width)
      ~height:(Int32.of_int t.height)
      ~stride:(Int32.of_int stride)
      ~format:Wl_shm.Format.Xrgb8888
    @@ object
      inherit [_] Wl_buffer.v1
      method on_release = Wl_buffer.destroy
    end
  in
  Wl_shm_pool.destroy pool;
  Wl_surface.attach t.surface ~buffer:(Some buffer) ~x:0l ~y:0l;
  let scroll = t.scroll in
  for row = 0 to t.height - 1 do
    for col = 0 to t.width - 1 do
      if (col + (row + scroll) land -16) land 31 < 16 then
        data.{row, col} <- 0xFF666666l
      else
        data.{row, col} <- t.fg
    done
  done;
  Wl_surface.damage t.surface ~x:0l ~y:0l ~width:Int32.max_int ~height:Int32.max_int;
  Wl_surface.commit t.surface

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level (Some Info));
  Printexc.record_backtrace true;
  Lwt_main.run begin
    (* Connect to the server. *)
    let* transport = Wayland.Unix_transport.connect () in
    let display, conn_closed = Wayland.Client.connect transport in
    Lwt.on_success conn_closed (function
        | Ok () -> ()
        | Error ex -> raise ex
      );
    (* Get the registry and find the objects we need. *)
    let* reg = Registry.of_display display in
    let compositor = Registry.bind reg (new Wl_compositor.v4) in
    let shm = Registry.bind reg @@ object
        inherit [_] Wl_shm.v1
        method on_format _ ~format:_ = ()
      end
    in
    let xdg_wm_base = Registry.bind reg @@ object
        inherit [_] Xdg_wm_base.v1
        method on_ping = Xdg_wm_base.pong
      end
    in
    let surface = Wl_compositor.create_surface compositor @@ object
        inherit [_] Wl_surface.v1
        method on_enter _ ~output:_ = ()
        method on_leave _ ~output:_ = ()
      end
    in
    let seat = Registry.bind reg @@ object
        inherit [_] Wl_seat.v1
        method on_capabilities _ ~capabilities:_ = ()
        method on_name _ ~name:_ = ()
      end
    in
    let t = { shm; surface; scroll = 0; vy = 1; width = 0; height = 0; fg = 0xFFEEEEEEl } in
    let closed, set_closed = Lwt.wait () in
    let _keyboard = Wl_seat.get_keyboard seat @@ object
        inherit [_] Wl_keyboard.v1
        method on_keymap    _ ~format:_ ~fd ~size:_ = Unix.close fd
        method on_enter     _ ~serial:_ ~surface:_ ~keys:_ = ()
        method on_leave     _ ~serial:_ ~surface:_ = ()
        method on_key       _ ~serial:_ ~time:_ ~key ~state =
          (* Change colour on key-presses *)
          if state = Wl_keyboard.Key_state.Pressed then
            t.fg <- Int32.(logand 0xffffffl (add t.fg (shift_left 0x101l (0xf land (Int32.to_int key)))))
        method on_modifiers _ ~serial:_ ~mods_depressed:_ ~mods_latched:_ ~mods_locked:_ ~group:_ = ()
        method on_repeat_info _ ~rate:_ ~delay:_ = ()
      end
    in
    let _pointer = Wl_seat.get_pointer seat @@ object
        inherit [_] Wl_pointer.v1
        method on_axis _ ~time:_ ~axis:_ ~value:_ = ()
        method on_button _ ~serial:_ ~time:_ ~button:_ ~state =
          match state with
          | Wl_pointer.Button_state.Pressed -> t.vy <- -t.vy
          | Wl_pointer.Button_state.Released -> ()
        method on_enter _ ~serial:_ ~surface:_ ~surface_x:_ ~surface_y:_ = ()
        method on_leave _ ~serial:_ ~surface:_ = ()
        method on_motion _ ~time:_ ~surface_x:_ ~surface_y:_ = ()
        method on_axis_source _ ~axis_source:_ = ()
        method on_axis_discrete _ ~axis:_ ~discrete:_ = ()
        method on_frame _ = ()
        method on_axis_stop _ ~time:_ ~axis:_ = ()
      end
    in
    let configured, set_configured = Lwt.wait () in
    let xdg_surface = Xdg_wm_base.get_xdg_surface xdg_wm_base ~surface @@ object
        inherit [_] Xdg_surface.v1
        method on_configure proxy ~serial =
          Xdg_surface.ack_configure proxy ~serial;
          if Lwt.is_sleeping configured then
            Lwt.wakeup set_configured ()
      end
    in
    let toplevel = Xdg_surface.get_toplevel xdg_surface @@ object
        inherit [_] Xdg_toplevel.v1
        method on_configure_bounds _ ~width:_ ~height:_ = ()
        method on_configure _ ~width ~height ~states:_ =
          t.width <- if width = 0l then 640 else Int32.to_int width;
          t.height <- if height = 0l then 480 else Int32.to_int height
        method on_close _ =
          Fmt.pr "Window closed - exiting!@.";
          Lwt.wakeup set_closed ()
        method on_wm_capabilities _ ~capabilities:_ = ()
      end
    in
    Xdg_toplevel.set_title toplevel ~title:"ocaml-wayland";
    Wl_surface.commit surface;
    let* () = configured in
    let rec animate () =
      let _frame = Wl_surface.frame surface (Wayland.callback ready) in
      draw_frame t
    and ready _ =
      t.scroll <- t.scroll + t.vy;
      animate ()
    in
    animate ();
    (* Wait until the user closes the window *)
    closed
  end
