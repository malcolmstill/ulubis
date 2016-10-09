# Ulubis

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
- [cl-libinput](https://github.com/malcolmstill/cl-egl) (currently a work in progress)

The dependencies for the SDL2 backend are:
- SDL2
- [cepl.sdl2](https://github.com/cbaggers/cepl.sdl2)

## Installation of xdg-shell helper

Currently there is a small helper library for xdg-shell which needs to be installed to `/usr/lib/` which can be done as follows:
```
> (ql:quickload :cl-wayland)
```
Navigate to the `cl-wayland` directory and run
```
> sh build-xdg-shell-shared-library.sh 
```
And move the generated shared library into `/usr/lib/`. In future `generate-bindings.lisp` will generate the small amount of code required.

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

