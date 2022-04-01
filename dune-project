(lang dune 2.8)
(name wayland)
(formatting disabled)
(generate_opam_files true)
(source (github talex5/ocaml-wayland))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")
(documentation "https://talex5.github.io/ocaml-wayland/")
(license "Apache-2.0 (excluding schema files)")
(package
 (name wayland)
 (synopsis "Pure OCaml Wayland protocol library")
 (description "Wayland is a communications protocol intended for use between processes on a single computer. It is mainly used by graphical applications (clients) to talk to display servers, but nothing about the protocol is specific to graphics and it could be used for other things. This library can be used to write Wayland clients, servers and proxies.")
 (depends
  (alcotest-lwt (and (>= 1.2.3) :with-test))
  (ocaml (>= 4.08.0))
  conf-pkg-config
  conf-wayland-protocols
  (xmlm (>= 1.3.0))
  (lwt (>= 5.4.0))
  (logs (>= 0.7.0))
  (fmt (>= 0.8.9))
  (cstruct (>= 6.0.0))
  (cmdliner (>= 1.0.4))))
