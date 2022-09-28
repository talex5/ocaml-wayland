open Lwt.Syntax
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

      method on_set_actions t ~dnd_actions:_ ~preferred_action =
        Wl_data_offer.action t ~dnd_action:preferred_action

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
      method on_add _ ~x ~y ~width ~height =
        rects := { x; y; width; height } :: !rects
      method on_destroy t = Proxy.delete t
      method on_subtract _ ~x:_ ~y:_ ~width:_ ~height:_ = failwith "Not implemented"
    end

  let make_surface t m =
    let sid = t.next_surface in
    t.next_surface <- t.next_surface + 1;
    log t "Created surface %d" sid;
    Proxy.Handler.attach m @@ object
      inherit [_] Wl_surface.v1
      method on_attach _ ~buffer:_ ~x:_ ~y:_ = failwith "Not implemented"
      method on_commit _ = failwith "Not implemented"
      method on_damage _ ~x:_ ~y:_ ~width:_ ~height:_ = failwith "Not implemented"
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

  let connect socket =
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
    let s : Server.t =
      Server.connect socket @@ object
        inherit [_] Wl_display.v1
        method on_sync _ cb =
          Proxy.Handler.attach cb @@ new Wl_callback.v1;
          Wl_callback.done_ cb ~callback_data:(next_serial ());
          Proxy.delete cb

        method on_get_registry _ reg =
          Proxy.Handler.attach reg @@ object
            inherit [_] Wl_registry.v1
            method on_bind : type a. _ -> name:int32 -> (a, [`Unknown], _) Proxy.t -> unit =
              fun _ ~name proxy ->
              match Proxy.ty proxy with
              | Wayland_proto.Wl_compositor.T ->
                assert (name = comp_name);
                make_compositor t proxy
              | _ -> Fmt.failwith "Invalid service name for %a" Proxy.pp proxy
          end;
          Wl_registry.global reg ~name:comp_name ~interface:"wl_compositor" ~version:1l
      end
    in
    Lwt.async (fun () ->
        Lwt.finalize
          (fun () ->
             let open Lwt.Infix in
             Server.closed s >>= function
             | Ok () -> Lwt.return_unit
             | Error ex -> raise ex
          )
          (fun () ->
             socket#close
          )
      );
    t

  let get_log t = List.rev t.log
end

let test_simple _ () =
  let test_over, set_test_over = Lwt.wait () in
  let socket_c, socket_s = Lwt_unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  let transport = Unix_transport.of_socket socket_c  in
  let c, conn_closed = Client.connect transport in
  Lwt.on_success conn_closed (function
      | Ok () -> Lwt.wakeup set_test_over ()
      | Error ex -> raise ex
    );
  let server = Unix_transport.of_socket socket_s |> S.connect in
  let open Wayland.Wayland_client in
  let* reg = Registry.of_display c in
  let comp = Registry.bind reg @@ new Wl_compositor.v1 in
  let surface = Wl_compositor.create_surface comp @@ object
      inherit [_] Wl_surface.v1
      method on_enter _ ~output:_ = ()
      method on_leave _ ~output:_ = ()
    end
  in
  let region = Wl_compositor.create_region comp @@ new Wl_region.v1 in
  Wl_region.add region ~x:10l ~y:20l ~width:30l ~height:40l;
  Wl_surface.set_input_region surface ~region:(Some region);
  let* () = Client.sync c in
  Alcotest.(check (list string)) "Check log" [
    "Created surface 1";
    "Surface 1 input region <- (10, 20)+(30, 40)";
  ] @@ S.get_log server;
  Alcotest.(check bool) "Still up" true (Proxy.transport_up region);
  let* () = transport#shutdown in
  Alcotest.(check bool) "Disconnecting" false (Proxy.transport_up region);
  let* () = test_over in
  transport#close

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level (Some Info));
  Lwt_main.run begin
    let open Alcotest_lwt in
    run "wayland" [
      "protocol", [
        test_case "simple" `Quick test_simple;
      ];
    ]
  end
