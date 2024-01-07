#!/bin/sh
wget https://gitlab.freedesktop.org/wayland/wayland-protocols/-/raw/main/stable/xdg-shell/xdg-shell.xml -O xdg-shell.xml
wget https://gitlab.freedesktop.org/wayland/wayland-protocols/-/raw/main/unstable/linux-dmabuf/linux-dmabuf-unstable-v1.xml -O linux-dmabuf-unstable-v1.xml
wget https://gitlab.freedesktop.org/wayland/wayland-protocols/-/raw/main/unstable/primary-selection/primary-selection-unstable-v1.xml -O primary-selection-unstable-v1.xml
wget https://gitlab.freedesktop.org/wayland/wayland-protocols/-/raw/main/unstable/relative-pointer/relative-pointer-unstable-v1.xml -O relative-pointer-unstable-v1.xml
wget https://gitlab.freedesktop.org/wayland/wayland-protocols/-/raw/main/unstable/pointer-constraints/pointer-constraints-unstable-v1.xml -O pointer-constraints-unstable-v1.xml
wget https://gitlab.freedesktop.org/wlroots/wlroots/-/raw/master/protocol/server-decoration.xml -O server-decoration.xml
wget https://cgit.freedesktop.org/mesa/mesa/plain/src/egl/wayland/wayland-drm/wayland-drm.xml -O wayland-drm.xml
wget https://gitlab.freedesktop.org/wayland/wayland-protocols/-/raw/main/unstable/xdg-decoration/xdg-decoration-unstable-v1.xml -O xdg-decoration-unstable-v1.xml
wget https://gitlab.freedesktop.org/wayland/wayland-protocols/-/raw/main/unstable/xdg-output/xdg-output-unstable-v1.xml -O xdg-output-unstable-v1.xml
wget https://gitlab.freedesktop.org/wlroots/wlr-protocols/-/raw/master/unstable/wlr-screencopy-unstable-v1.xml -O wlr-screencopy-unstable-v1.xml
wget https://gitlab.freedesktop.org/wlroots/wlr-protocols/-/raw/master/unstable/wlr-layer-shell-unstable-v1.xml -O wlr-layer-shell-unstable-v1.xml
wget https://gitlab.freedesktop.org/wayland/wayland/-/raw/main/protocol/wayland.xml -O ../lib/wayland.xml
