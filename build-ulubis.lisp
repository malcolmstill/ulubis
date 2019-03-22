
(ql:quickload :cffi)
(ql:quickload :cepl)
(ql:quickload :easing)
(ql:quickload :osicat)
(ql:quickload :trivial-dump-core)
(ql:quickload :uiop)
(ql:quickload :closer-mop)
(ql:quickload :swank)
(ql:quickload :rtg-math)
(ql:quickload :trivial-backtrace)
(ql:quickload :cl-cairo2)
(ql:quickload :zpng)

(load "cl-drm/cl-drm.asd")
(asdf:load-system :cl-drm)

(load "cl-gbm/cl-gbm.asd")
(asdf:load-system :cl-gbm)

(load "cl-egl/cl-egl.asd")
(asdf:load-system :cl-egl)

(load "cl-libinput/cl-libinput.asd")
(asdf:load-system :cl-libinput)

(load "cepl.drm-gbm/cepl.drm-gbm.asd")
(asdf:load-system :cepl.drm-gbm)

(load "cl-wayland/cl-wayland.asd")
(asdf:load-system :cl-wayland)

(load "cl-xkb/cl-xkb.asd")
(asdf:load-system :cl-xkb)

(load "ulubis.asd")
(asdf:load-system :ulubis)

(load "ulubis-drm-gbm/ulubis-drm-gbm.asd")
(asdf:load-system :ulubis-drm-gbm)

(trivial-dump-core:save-executable "ulubis" #'ulubis::run-compositor)

(quit)
