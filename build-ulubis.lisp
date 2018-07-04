
(ql:quickload :cffi)
(ql:quickload :cepl)
(ql:quickload :easing)
(ql:quickload :osicat)
(ql:quickload :trivial-dump-core)
(ql:quickload :uiop)

(ql:quickload "cl-drm")
(ql:quickload "cl-gbm")
(ql:quickload "cl-egl")
(ql:quickload "cl-libinput")
(ql:quickload "cepl.drm-gbm")
(ql:quickload "cl-wayland")
(ql:quickload "cl-xkb")
(ql:quickload "ulubis")
(ql:quickload "ulubis-drm-gbm")

(trivial-dump-core:save-executable "ulubis" #'ulubis::run-compositor)

(quit)

