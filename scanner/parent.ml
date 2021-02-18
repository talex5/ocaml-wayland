open Schema

module Interface_map = Map.Make(String)

type t = {
  mutable parent : Interface.t Interface_map.t;
}

let index_ops t (parent : Interface.t) (msg : Message.t) =
  msg.args |> List.iter (fun (arg : Arg.t) ->
      match arg.ty with
      | `New_ID (Some "wl_callback") -> ()  (* Seems to be an exception to the tree rule *)
      | `New_ID (Some interface) ->
        Interface_map.find_opt interface t.parent |> Option.iter (fun (other_parent : Interface.t) ->
            if other_parent.name <> parent.name then (
              Fmt.pr "WARNING: Interface %S has two creation parents: %S and %S!@."
                interface parent.name other_parent.name
            )
          );
        t.parent <- Interface_map.add interface parent t.parent
      | _ -> ()
    )

let index (p : Protocol.t) =
  let t = { parent = Interface_map.empty } in
  p.interfaces |> List.iter (fun (iface : Interface.t) ->
      List.iter (index_ops t iface) iface.events;
      List.iter (index_ops t iface) iface.requests
    );
  t

let parent t (child : Interface.t) =
  Interface_map.find_opt child.name t.parent
