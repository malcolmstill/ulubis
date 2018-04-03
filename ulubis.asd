;;;; ulubis.asd

(asdf:defsystem #:ulubis
  :description "A Common Lisp Wayland compositor"
  :author "Malcolm Still"
  :license "BSD 3-Clause"
  :depends-on (#:cffi
	       #:osicat
	       #:swank
	       #:cepl
               #:rtg-math
               #:rtg-math.vari
	       #:easing
	       #:cl-xkb
               #:cl-wayland
	       #:trivial-dump-core
	       #:trivial-backtrace
               #:cl-cairo2
	       #:uiop)
  :serial t
  :components ((:file "backend")
	       (:file "syscall")
	       (:file "animation")
	       (:file "package")
               (:file "cairo-surface")
	       (:file "ianimatable")
	       ;; (:file "isurface")
	       (:file "client")
	       (:file "compositor")
	       ;; (:file "plumbing")
	       (:file "wl-surface-impl")
	       (:file "wl-region-impl")
	       (:file "wl-compositor-impl")
	       (:file "wl-data-device-impl")
	       (:file "wl-data-source-impl")
	       (:file "wl-data-device-manager-impl")
	       (:file "wl-output-impl")
	       (:file "wl-keyboard-impl")
	       (:file "wl-pointer-impl")
	       (:file "wl-seat-impl")
	       (:file "wl-shell-surface-impl")
	       (:file "wl-shell-impl")
	       (:file "wl-subsurface-impl")
	       (:file "wl-subcompositor-impl")
	       (:file "zxdg-toplevel-v6-impl")
	       (:file "zxdg-surface-v6-impl")
	       (:file "zxdg-shell-v6-impl")
	       (:file "xdg-surface-impl")
	       (:file "xdg-shell-impl")
	       (:file "render")
	       (:file "mode")
	       (:file "view")
	       (:file "desktop-mode")
	       (:file "alt-tab-mode")
	       (:file "ulubis")
	       (:file "install")))

