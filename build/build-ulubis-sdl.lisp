(ql:quickload :ulubis)
(ql:quickload :ulubis-sdl)

(sb-ext:save-lisp-and-die "ulubis-sdl"
			  :executable t
			  :toplevel #'ulubis::run-compositor)

(quit)
