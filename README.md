# Ulubis

[![Join the chat at https://gitter.im/ulubis/Lobby](https://badges.gitter.im/ulubis/Lobby.svg)](https://gitter.im/ulubis/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

![Ulubis in action](https://github.com/malcolmstill/ulubis/raw/master/ulubis.gif)

Ulubis is a Wayland compositor written in Common Lisp. It is inspired by FVWM and StumpWM. The idea is that it is easy to hack on, customise, define your own interaction modes, etc. (see [alt-tab-mode.lisp](https://github.com/malcolmstill/ulubis/blob/master/alt-tab-mode.lisp) as an example of defining a custom mode)

(I currently call it a compositor intentionally...let's get a bit more window management functionality in before calling it a WM)

## Status

Ulubis is known to work with sbcl and ccl. I have only tested it on two machines which Intel graphics chips, please get in touch if it does / doesn't work with Nvidia or AMD cards. It is very alpha.

## Roadmap

The vague roadmap for ulubis is as follows (not necessarily in order):
- [] Add (an at least rudimentary) menu system
- [] Server-side decorations
- [] Add screen locking
- [] Add screenshooting and video capture
- [] Support multiple monitors
- [] Support more (most?) Wayland clients
- [] XWayland support
- [] Potentially define custom Wayland protocols for ulubis (maybe you want to replace a built-in menu with your own menu written in QML)

## Dependencies

Ulubis depends on:
- libwayland
- [cl-wayland](https://github.com/malcolmstill/cl-wayland)
- libxkbcommon
- [cl-xkb](https://github.com/malcolmstill/cl-xkb)
- [cepl](https://github.com/cbaggers/cepl)
- [vydd's easing library](https://github.com/vydd/easing)
- [osicat](https://github.com/osicat/osicat)

Ulubis has two backends: [ulubis-sdl](https://github.com/malcolmstill/ulubis-sdl) (an SDL2 backend) and [ulubis-drm-gbm](https://github.com/malcolmstill/ulubis-drm-gbm) (a DRM/GBM backend). The DRM/GBM backend is intended to be *the* backend whilst the SDL2 is intended for testing on X.

The DRM/GBM backend depends on:
- libdrm 
- libgbm 
- libEGL
- [cl-drm](https://github.com/malcolmstill/cl-drm)
- [cl-gbm](https://github.com/malcolmstill/cl-gbm)
- [cl-egl](https://github.com/malcolmstill/cl-egl)
- [cepl.drm-gbm](https://github.com/malcolmstill/cepl.drm-gbm)
- [cl-libinput](https://github.com/malcolmstill/cl-libinput)

The dependencies for the SDL2 backend are:
- SDL2
- [cepl.sdl2](https://github.com/cbaggers/cepl.sdl2)

## Installation of ulubis (the following will work pending inclusion of ulubis on quicklisp)

From a terminal run sbcl (or ccl) and issue the commands `(ql:quickload :ulubis)` followed by `(ulubis::build)`, for example (you may see more output):
```
[malcolm@sense-amid-madness-wit-amidst-folly cl-libinput]$ sbcl
This is SBCL 1.3.4-1.fc24, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (ql:quickload :ulubis)
To load "ulubis":
  Load 1 ASDF system:
    ulubis
; Loading "ulubis"
...............................................
; recompile cpu side of (CURSOR-PIPELINE ...)
..
; recompile cpu side of (DEFAULT-PIPELINE ...)
....
; recompile cpu side of (ALT-TAB-PIPELINE ...)
.
(:ULUBIS)
* (ulubis::build)
To load "ulubis-drm-gbm":
  Load 1 ASDF system:
    ulubis-drm-gbm
; Loading "ulubis-drm-gbm"
.
Building ulubis with DRM backend
[undoing binding stack and other enclosing state... done]
[saving current Lisp image into ulubis:
writing 4800 bytes from the read-only space at 0x20000000
writing 18192 bytes from the static space at 0x20100000
writing 92176384 bytes from the dynamic space at 0x1000000000
done]
```
This will generate the `ulubis` executable in the current directory which uses the DRM backend.

Alternatively you can issues `(ulubis::build-sdl)` in place of `(ulubis::build)` to generate an executable that uses the SDL backend which can be run on X.

## Running ulubis

To run `ulubis` the user must be a member of the `input` and `video` groups. Navigate to a virtual terminal and run

```
> ulubis
```

For the SDL2 backend simply run `ulubis-sdl` when in X.

## Configuration

Ulubis looks for the file `~/.ulubis.lisp` and loads it if it exists.

An example configuration is as follows:

```
(in-package :ulubis)

(if (string-equal (symbol-name ulubis-backend:backend-name) "backend-drm-gbm")
    (progn
      (setf (screen-width *compositor*) 1440)
      (setf (screen-height *compositor*) 900))
    (progn
      (setf (screen-width *compositor*) 900)
      (setf (screen-height *compositor*) 600)))

(set-keymap *compositor* "evdev" "apple" "gb" "" "")

(defun startup ()
  (swank-loader:init)
  (swank:create-server :port 4005 :style :spawn :dont-close t)
  (swank:set-package "ULUBIS")

  ;; Add 4 views (virtual desktops) using the desktop-mode as default
  (loop :for i :from 0 :to 3 :do (push-view 'desktop-mode))
  (setf (current-view *compositor*) (first (views *compositor*))))

```
