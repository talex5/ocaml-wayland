# v2.1

- Add relative-pointer-v1 (@Mic92 #40).

- Add pointer-constraints-unstable-v1 (@Mic92 #41).

- Update protocols to latest versions (@talex5 #43).

- Cope with interfaces that are always v1 (@talex5 #44).

# v2.0

- Convert from Lwt to Eio (@talex5 #37).  
  Remove `set_paused`, as it's no longer needed.

- Add `Client.stop` and `Server.stop` (@talex5 #37).  
  These allow shutting down the connection without reporting an error.

- Update to latest versions of Wayland protocols (@talex5 #38).

- Add layer shell protocol (@yilinwei #36).

# v1.1

- Add update.sh script and update protocols (@MisterDA #25, @talex5 #33).

- Improve proxy module documentation (@talex5 #31).

- Improve "No such object" error (@talex5 #28).

- Make `Handler.cast_version` more general (@talex5 #27).  
  Avoids the need to cast to `Handler.t` before using `cast_version`.

- Update to cmdliner.1.1.1 (@MisterDA #26).

# v1.0

- Improve connection shutdown (#22).
  Add `shutdown` and `up` methods to the transport API.
  Call `shutdown` to start a clean shutdown of the connection.
  Use `up` to detect that a connection is shutting down (to avoid logging pointless errors).
  Add `close` and `socket` methods to `unix_transport`.

- Log exceptions handling messages instead of aborting the connection (#21).
  Most errors are not fatal, and closing the connection means the application can't even e.g. prompt the user to save their work.

- Add `Client.dump` and `Server.dump` (#20). Useful for finding object reference leaks.

- Add `Proxy.pp_transport` (#19). Useful for logging when you have multiple connections.

- Also call `on_delete` handlers when the connection ends (#18).
  Allows them to be used for cleaning up resources.

- Add `set_paused` to pause processing of incoming messages (#17).

- Add `Proxy.can_send` (#16).
  This is useful to check whether a proxy has been destroyed.

- Raise `Invalid_argument` on `Proxy.id` if the proxy is destroyed.

- Add `wp_primary_selection_unstable_v1`.
  This appears to be identical to gtk-primary-selection except for the name and that it's marked as unstable.
  But sway has dropped support for the old name (see https://github.com/swaywm/sway/pull/5788).

- Update to latest Fmt API.

- Fix deprecation warning with cstruct 6.0.1.

# v0.2

- Bugfix: handle `SIGPIPE` properly (#15).
- Add license field to opam metadata.

# v0.1

Initial release.
