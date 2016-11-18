;;;; package.lisp

(defpackage :ulubis
  (:use :common-lisp
	:cffi
	:cepl
	:varjo-lang
	:wayland-util
	:wayland-server-core
	:wayland-server-protocol
	:xdg-shell-server-protocol
	:ulubis-backend
	:animation))
