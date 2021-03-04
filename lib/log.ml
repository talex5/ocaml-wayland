let src = Logs.Src.create "wayland" ~doc:"Wayland protocol library"
include (val Logs.src_log src : Logs.LOG)
