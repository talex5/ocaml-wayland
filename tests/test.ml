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
    mutable next_surface : int;
    mutable log : string list;
  }

  open Wayland.Wayland_server

  type region_data = rect list ref

  type 'a user_data = 
    | Region : region_data -> [`Wl_region] user_data

  type 'a Wayland.S.user_data += Server of 'a user_data

  let user_data (proxy : ('a, _) Proxy.t) : 'a user_data =
    match Wayland.Proxy.user_data proxy with
    | Server x -> x
    | S.No_data -> Fmt.failwith "No data attached to %a!" Proxy.pp proxy
    | _ -> Fmt.failwith "Unexpected data attached to %a!" Proxy.pp proxy

  let comp_name = 1l

  let log t fmt =
    fmt |> Fmt.kstrf @@ fun msg ->
    Logs.info (fun f -> f "Server: %s" msg);
    t.log <- msg :: t.log

  let make_region r =
    let rects = ref [] in
    Proxy.Handler.attach r @@ Wl_region.v1 ~user_data:(Server (Region rects)) @@ object
      method on_add _ ~x ~y ~width ~height =
        rects := { x; y; width; height } :: !rects
      method on_destroy _ = ()
      method on_subtract _ ~x:_ ~y:_ ~width:_ ~height:_ = failwith "Not implemented"
    end

  let make_surface t m =
    let sid = t.next_surface in
    t.next_surface <- t.next_surface + 1;
    log t "Created surface %d" sid;
    Proxy.Handler.attach m @@ Wl_surface.v1 @@ object
      method on_attach _ ~buffer:_ ~x:_ ~y:_ = failwith "Not implemented"
      method on_commit _ = failwith "Not implemented"
      method on_damage _ ~x:_ ~y:_ ~width:_ ~height:_ = failwith "Not implemented"
      method on_destroy _ = failwith "Not implemented"
      method on_frame _ _callback = failwith "Not implemented"
      method on_set_input_region _ ~region =
        let Region r = user_data region in
        log t "Surface %d input region <- %a" sid (Fmt.(list ~sep:comma) pp_rect) !r
      method on_set_opaque_region _ ~region:_ = failwith "Not implemented"
    end

  let make_compositor t proxy =
    let _ : _ Proxy.t = Proxy.Service_handler.attach proxy @@ Wl_compositor.v1 @@ object
        method on_create_region _ region = make_region region
        method on_create_surface _ surface = make_surface t surface
      end
    in
    ()

  let connect socket =
    let t = {
      next_surface = 1;
      log = [];
    } in
    let _ : Server.t =
      Server.connect socket (fun reg ->
          Proxy.Handler.attach reg @@ Wl_registry.v1 @@ object
            method on_bind : type a. _ -> name:int32 -> (a, [`Unknown]) Proxy.t -> unit =
              fun _ ~name proxy ->
              match Proxy.ty proxy with
              | Wayland_proto.Wl_compositor.T ->
                assert (name = comp_name);
                make_compositor t proxy
              | _ -> Fmt.failwith "Invalid service name for %a" Proxy.pp proxy
          end;
          Wl_registry.global reg ~name:comp_name ~interface:"wl_compositor" ~version:1l
        )
    in t

  let get_log t = List.rev t.log
end

let test_simple _ () =
  let socket_c, socket_s = Lwt_unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  let c = Unix_transport.of_socket socket_c |> Display.connect in
  let server = Unix_transport.of_socket socket_s |> S.connect in
  let open Wayland.Wayland_client in
  let* reg = Registry.of_display c in
  let comp = Registry.bind reg @@ Wl_compositor.v1 () in
  let surface = Wl_compositor.create_surface comp @@ Wl_surface.v1 @@ object
      method on_enter _ ~output:_ = ()
      method on_leave _ ~output:_ = ()
    end
  in
  let region = Wl_compositor.create_region comp @@ Wl_region.v1 () in
  Wl_region.add region ~x:10l ~y:20l ~width:30l ~height:40l;
  Wl_surface.set_input_region surface ~region;
  let* () = Display.sync c in
  Alcotest.(check (list string)) "Check log" [
    "Created surface 1";
    "Surface 1 input region <- (10, 20)+(30, 40)";
  ] @@ S.get_log server;
  Lwt.return ()

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
