;;;; package.lisp

(defpackage :ulubis
  (:use :common-lisp
	:cffi
	:cepl
	:wayland-util
	:wayland-server-core
	:wayland-server-protocol
	;;:xdg-shell-interface
	:xdg-shell-server-protocol
	:ulubis-backend
	:animation))
