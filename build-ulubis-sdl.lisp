
(ql:quickload :cffi)
(ql:quickload :cepl)
(ql:quickload :easing)
(ql:quickload :osicat)
(ql:quickload :trivial-dump-core)
(ql:quickload :uiop)
(ql:quickload :sdl2)
(ql:quickload :zpng)

(ql:quickload "cepl.sdl2")
(ql:quickload "cl-wayland")
(ql:quickload "ulubis")
(ql:quickload "ulubis-sdl")

(trivial-dump-core:save-executable "ulubis-sdl" #'ulubis::run-compositor)

(quit)

