let scan opens internal path =
  let ch = open_in_bin path in
  match Xml.parse ~name:path ch Schema.Protocol.parse with
  | exception Failure msg -> Fmt.epr "%s@." msg; exit 1
  | protocol -> Generate.output ~opens ~internal protocol

open Cmdliner

let spec_file =
  let doc = "The XML file to process." in
  Arg.(required @@ pos 0 (some file) None @@ info [] ~doc ~docv:"SPEC-XML")

let internal =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"For internal use only"
    ["internal"]

let opens =
  Arg.value @@
  Arg.(opt (list string)) [] @@
  Arg.info
    ~doc:"Extra modules to open"
    ["open"]

let scan = Term.(const scan $ opens $ internal $ spec_file)

let term_exit (x : unit Term.result) = Term.exit x

let () =
  term_exit @@ Term.eval (scan, Term.info "wayland-scanner-ocaml")
