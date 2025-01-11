open Eio.Std
open Wayland

type rect = {
  x : int32;
  y : int32;
  width : int32;
  height : int32;
}

let pp_rect f {x; y; width; height} = Fmt.pf f "(%ld, %ld)+(%ld, %ld)" x y width height

module S = struct
  type t = {
    mutable serial : int32;
    mutable next_surface : int;
    mutable log : string list;
  }

  open Wayland.Wayland_server

  type region_data = rect list ref

  type 'a user_data = 
    | Region : region_data -> [`Wl_region] user_data

  type ('a, 'role) Wayland.S.user_data += Server of 'a user_data

  let user_data (proxy : ('a, _, 'role) Proxy.t) : 'a user_data =
    match Wayland.Proxy.user_data proxy with
    | Server x -> x
    | S.No_data -> Fmt.failwith "No data attached to %a!" Proxy.pp proxy
    | _ -> Fmt.failwith "Unexpected data attached to %a!" Proxy.pp proxy

  let comp_name = 1l

  let log t fmt =
    fmt |> Fmt.kstr @@ fun msg ->
    Logs.info (fun f -> f "Server: %s" msg);
    t.log <- msg :: t.log

  (* Even though we use [Wl_data_offer.action], which is a V3 API, we only do that
     in response to a V3 request. So the handler can actually handle all versions.
     This is just here to check that it compiles. *)
  let _make_data_offer (m : [`V1 | `V2 | `V3] Wl_data_offer.t) =
    Proxy.Handler.attach m @@ object
      inherit [_] Wl_data_offer.v1

      method on_set_actions t ~untrusted_dnd_actions:_ ~untrusted_preferred_action =
        Wl_data_offer.action t ~dnd_action:untrusted_preferred_action

      method on_receive _ = failwith "Not implemented"
      method on_finish _ = failwith "Not implemented"
      method on_destroy _ = failwith "Not implemented"
      method on_accept _ = failwith "Not implemented"
    end

  let make_region r =
    let rects = ref [] in
    Proxy.Handler.attach r @@ object
      inherit [_] Wl_region.v1
      method! user_data = Server (Region rects)
      method on_add _ ~untrusted_x ~untrusted_y ~untrusted_width ~untrusted_height =
        rects := { x = untrusted_x; y = untrusted_y; width = untrusted_width; height = untrusted_height } :: !rects
      method on_destroy t = Proxy.delete t
      method on_subtract _ ~untrusted_x:_ ~untrusted_y:_ ~untrusted_width:_ ~untrusted_height:_ = failwith "Not implemented"
    end

  let make_surface t m =
    let sid = t.next_surface in
    t.next_surface <- t.next_surface + 1;
    log t "Created surface %d" sid;
    Proxy.Handler.attach m @@ object
      inherit [_] Wl_surface.v1
      method on_attach _ ~buffer:_ ~untrusted_x:_ ~untrusted_y:_ = failwith "Not implemented"
      method on_commit _ = failwith "Not implemented"
      method on_damage _ ~untrusted_x:_ ~untrusted_y:_ ~untrusted_width:_ ~untrusted_height:_ = failwith "Not implemented"
      method on_destroy = failwith "Not implemented"
      method on_frame _ _callback = failwith "Not implemented"
      method on_set_input_region _ ~region =
        match region with
        | None -> log t "Surface %d input region cleared" sid
        | Some region ->
          let Region r = user_data region in
          log t "Surface %d input region <- %a" sid (Fmt.(list ~sep:comma) pp_rect) !r
      method on_set_opaque_region _ ~region:_ = failwith "Not implemented"
      method on_set_buffer_transform = failwith "Not implemented"
      method on_set_buffer_scale = failwith "Not implemented"
      method on_damage_buffer = failwith "Not implemented"
      method on_offset = failwith "Not implemented"
    end

  let make_compositor t proxy =
    Proxy.Service_handler.attach proxy @@ object
      inherit [_] Wl_compositor.v1
      method on_create_region _ region = make_region region
      method on_create_surface _ surface = make_surface t surface
    end

  let connect ~sw socket =
    let t = {
      serial = 0l;
      next_surface = 1;
      log = [];
    } in
    let next_serial () =
      let i = t.serial in
      t.serial <- Int32.succ i;
      i
    in
    let s =
      Server.connect ~sw socket @@ object
        inherit [_] Wl_display.v1
        method on_sync _ cb =
          Proxy.Handler.attach cb @@ new Wl_callback.v1;
          Wl_callback.done_ cb ~callback_data:(next_serial ());
          Proxy.delete cb

        method on_get_registry _ reg =
          Proxy.Handler.attach reg @@ object
            inherit [_] Wl_registry.v1
            method on_bind : type a. _ -> untrusted_name:int32 -> (a, [`Unknown], _) Proxy.t -> unit =
              fun _ ~untrusted_name proxy ->
              match Proxy.ty proxy with
              | Wayland_proto.Wl_compositor.T ->
                assert (untrusted_name = comp_name);
                make_compositor t proxy
              | _ -> Fmt.failwith "Invalid service name for %a" Proxy.pp proxy
          end;
          Wl_registry.global reg ~name:comp_name ~interface:"wl_compositor" ~version:1l
      end
    in
    ignore (s : Server.t);
    t

  let get_log t = List.rev t.log
end

let test_simple () =
  Switch.run @@ fun sw ->
  let socket_c, socket_s = Eio_unix.Net.socketpair_stream ~sw () in
  let transport = Unix_transport.of_socket socket_c  in
  let c = Client.connect ~sw transport in
  let server = Unix_transport.of_socket socket_s |> S.connect ~sw in
  let open Wayland.Wayland_client in
  let reg = Registry.of_display c in
  let comp = Registry.bind reg @@ new Wl_compositor.v1 in
  let surface = Wl_compositor.create_surface comp @@ object
      inherit [_] Wl_surface.v1
      method on_enter _ ~output:_ = ()
      method on_leave _ ~output:_ = ()
      method on_preferred_buffer_scale _ ~factor:_ = ()
      method on_preferred_buffer_transform _ ~transform:_ = ()
    end
  in
  let region = Wl_compositor.create_region comp @@ new Wl_region.v1 in
  Wl_region.add region ~x:10l ~y:20l ~width:30l ~height:40l;
  Wl_surface.set_input_region surface ~region:(Some region);
  Client.sync c;
  Alcotest.(check (list string)) "Check log" [
    "Created surface 1";
    "Surface 1 input region <- (10, 20)+(30, 40)";
  ] @@ S.get_log server;
  Alcotest.(check bool) "Still up" true (Proxy.transport_up region);
  Client.stop c;
  Alcotest.(check bool) "Disconnecting" false (Proxy.transport_up region)

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level (Some Info));
  Eio_main.run @@ fun _env ->
  let open Alcotest in
  run "wayland" [
    "protocol", [
      test_case "simple" `Quick test_simple;
    ];
  ]
