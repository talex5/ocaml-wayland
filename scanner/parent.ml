open Schema

module Interface_map = Map.Make(String)

type t = {
  mutable parent : (Interface.t * [`Request | `Event]) Interface_map.t;
  (** Says which interface creates this object, and whether its done by a request or an event. *)
}

let index_ops ~msg_type t (parent : Interface.t) (msg : Message.t) =
  msg.args |> List.iter (fun (arg : Arg.t) ->
      match arg.ty with
      | `New_ID (Some interface) ->
        Interface_map.find_opt interface t.parent |> Option.iter (fun ((other_parent : Interface.t), other_msg_type) ->
            (* wl_callback seems to be an exception to the tree rule *)
            if other_parent.name <> parent.name && interface <> "wl_callback" then (
              Fmt.pr "WARNING: Interface %S has two creation parents: %S and %S!@."
                interface parent.name other_parent.name
            );
            if other_msg_type <> msg_type then
              Fmt.pr "WARNING: Interface %S is created by both clients and servers!" interface;
          );
        t.parent <- Interface_map.add interface (parent, msg_type) t.parent
      | _ -> ()
    )

let index (p : Protocol.t) =
  let t = { parent = Interface_map.empty } in
  p.interfaces |> List.iter (fun (iface : Interface.t) ->
      List.iter (index_ops t iface ~msg_type:`Event) iface.events;
      List.iter (index_ops t iface ~msg_type:`Request) iface.requests
    );
  t

let parent t (child : Interface.t) =
  Interface_map.find_opt child.name t.parent
