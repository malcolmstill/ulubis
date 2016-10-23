(ql:quickload :ulubis-drm-gbm)

(format t "Building ulubis with DRM backend~%")

(trivial-dump-core:save-executable "ulubis" #'ulubis::run-compositor)

(quit)
