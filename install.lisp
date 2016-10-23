
(in-package :ulubis)

(defun build (&optional path)
  (let ((ulubis-path (namestring (ql:where-is-system :ulubis))))
    (load (concatenate 'string ulubis-path "build/build-ulubis-drm-gbm.lisp"))))

(defun build-sdl (&optional path)
  (let ((ulubis-path (namestring (ql:where-is-system :ulubis))))
    (load (concatenate 'string ulubis-path "build/build-ulubis-sdl.lisp"))))
