# Ulubis

![Ulubis in action](https://github.com/malcolmstill/ulubis/raw/master/ulubis.gif)

Ulubis is a Wayland compositor written in Common Lisp. It currently only works with SBCL. Rendering is done with OpenGL via cbaggers' fantastic CEPL library.

## Dependencies

Ulubis depends on:
- libwayland
- [cl-wayland](https://github.com/malcolmstill/cl-wayland)
- libXKB
- [cl-xkb](https://github.com/malcolmstill/cl-xkb)
- [cepl](https://github.com/cbaggers/cepl)
- [vydd's easing library](https://github.com/vydd/easing)

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

## Installation of ulubis

Installation in the future (when everything is available on quicklisp) will be
```
> (ql:quickload :ulubis)
```

Then `cd` to the ulubis directory and run
```
> sh build/build-ulubis-drm-gbm.sh
```
or
```
> sh build/build-ulubis-sdl.sh
```
which will generate an `ulubis` and `ulubis-sdl` executable in the build directory.

## Running ulubis

To run `ulubis` the user must be a member of the `input` and `video` groups. Navigate to a virtual terminal and run `ulubis`.

For the SDL2 backend simply run `ulubis-sdl` when in X.

## Configuration

Ulubis looks for the file `~/.ulubis.lisp` and loads it if it exists.

An example configuration is as follows:

```
(in-package :ulubis)

;; Load swank so we can slime-connect
(swank-loader:init)
(swank:create-server :port 4005 :dont-close t)
(swank:set-package "ULUBIS")

;; Set resolution based on backend
(if (equal backend-name 'ulubis-backend::backend-drm-gbm)
    (progn
      (setf (screen-width *compositor*) 1440)
      (setf (screen-height *compositor*) 900))
    (progn
      (setf (screen-width *compositor*) 1200)
      (setf (screen-height *compositor*) 800)))

;; Set keymap in RMLVO format
(set-keymap *compositor*
	    "evdev"
	    "apple"
	    "gb"
	    ""
	    "")

;; Only for DRM/GBM backend, set the device paths
;; for libinput (on my machine event5 is the keboard
;; and event8 is the mouse)
(setf (devices *compositor*) (list
			      "/dev/input/event5"
			      "/dev/input/event8"))

```
