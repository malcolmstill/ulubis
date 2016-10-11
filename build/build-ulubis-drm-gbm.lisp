(ql:quickload :ulubis)
(ql:quickload :ulubis-drm-gbm)

(sb-ext:save-lisp-and-die "ulubis"
			  :executable t
			  :toplevel #'ulubis::run-compositor)

(quit)
