;;;; package.lisp

(defpackage :ulubis
  (:use :common-lisp
	:cffi
	:cepl
	:wayland-server-core
	:wayland-server-protocol
	:xdg-shell-server-protocol
	:ulubis-backend
	:animation))
