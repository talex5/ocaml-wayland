let src = Logs.Src.create "wayland" ~doc:"Wayland protocol"
include (val Logs.src_log src : Logs.LOG)
