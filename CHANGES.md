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
