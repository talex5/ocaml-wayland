let bool_of_string = function
  | "true" -> true
  | "false" -> false
  | x -> Fmt.failwith {|Expected "true" or "false", but got %S|} x

let int_of_string x v =
  match int_of_string_opt v with
  | Some i -> i
  | None -> Fmt.failwith "Not an integer %S at %a" v Xml.pp x

module Description = struct
  type t = {
    summary : string;
    full : string;
  }

  let parse x =
    {
      summary = Xml.take_attr "summary" x;
      full = Xml.take_data x;
    }
end

module Entry = struct
  type t = {
    name : string;
    value : string;
    summary : string option;
    since : int;
    description : Description.t option;
  }

  let parse x =
    {
      name = Xml.take_attr "name" x;
      value = Xml.take_attr "value" x;
      summary = Xml.take_attr_opt "summary" x;
      since = Xml.take_attr_opt "since" x |> Option.value ~default:"1" |> int_of_string x;
      description = Xml.take_sole_opt "description" x Description.parse;
    }
end

module Enum = struct
  type t = {
    name : string;
    since : int;
    bitfield : bool;
    description : Description.t option;
    entries : Entry.t list;
  }

  let parse x =
    {
      name = Xml.take_attr "name" x;
      since = Xml.take_attr_opt "since" x |> Option.value ~default:"1" |> int_of_string x;
      bitfield = Xml.take_attr_opt "bitfield" x |> Option.map bool_of_string |> Option.value ~default:false;
      description = Xml.take_sole_opt "description" x Description.parse;
      entries = Xml.take_elements "entry" x Entry.parse;
    }
end

module Arg = struct
  type ty = [ `Object of string option | `New_ID of string option | `Uint | `Int | `String | `Fixed | `FD | `Array ]

  type t = {
    name : string;
    ty : ty;
    summary : string option;
    allow_null : bool;
    enum : string option;
    description : Description.t option;
  }

  let parse_type x = function
    | "object" -> `Object (Xml.take_attr_opt "interface" x)
    | "new_id" -> `New_ID (Xml.take_attr_opt "interface" x)
    | "uint" -> `Uint
    | "int" -> `Int
    | "string" -> `String
    | "fixed" -> `Fixed
    | "fd" -> `FD
    | "array" -> `Array
    | v -> Fmt.failwith "Unknown message type %S at %a" v Xml.pp x

  let parse x =
    {
      name = Xml.take_attr "name" x;
      ty = Xml.take_attr "type" x |> parse_type x;
      summary = Xml.take_attr_opt "summary" x;
      allow_null = Xml.take_attr_opt "allow-null" x |> Option.map bool_of_string |> Option.value ~default:false;
      enum = Xml.take_attr_opt "enum" x;
      description = Xml.take_sole_opt "description" x Description.parse;
    }
end

module Message = struct
  type t = {
    name : string;
    description : Description.t option;
    ty : [`Normal | `Destructor];
    since : int;
    args : Arg.t list;
  }

  let parse_type x = function
    | Some "destructor" -> `Destructor
    | None -> `Normal
    | Some v -> Fmt.failwith "Unknown message type %S at %a" v Xml.pp x

  let parse x =
    {
      name = Xml.take_attr "name" x;
      ty = Xml.take_attr_opt "type" x |> parse_type x;
      since = Xml.take_attr_opt "since" x |> Option.value ~default:"1" |> int_of_string x;
      description = Xml.take_sole_opt "description" x Description.parse;
      args = Xml.take_elements "arg" x Arg.parse;
    }
end

module Interface = struct
  type t = {
    name : string;
    version : int;
    description : Description.t option;
    requests : Message.t list;
    events : Message.t list;
    enums : Enum.t list;
  }

  let parse x =
    {
      name = Xml.take_attr "name" x;
      version = Xml.take_attr "version" x |> int_of_string x;
      description = Xml.take_sole_opt "description" x Description.parse;
      requests = Xml.take_elements "request" x Message.parse;
      events = Xml.take_elements "event" x Message.parse;
      enums = Xml.take_elements "enum" x Enum.parse;
    }
end

module Protocol = struct
  type t = {
    name : string;
    copyright : string option;
    description : Description.t option;
    interfaces : Interface.t list;
  }

  let parse x =
    {
      name = Xml.take_attr "name" x;
      copyright = Xml.take_sole_opt "copyright" x Xml.take_data;
      description = Xml.take_sole_opt "description" x Description.parse;
      interfaces = Xml.take_elements "interface" x Interface.parse;
    }
end
