;;;; ulubis.asd

(asdf:defsystem #:ulubis
  :description "A Common Lisp Wayland compositor"
  :author "Malcolm Still"
  :license "BSD 3-Clause"
  :depends-on (#:cffi
	       #:swank
	       #:cepl
	       #:easing
	       #:cl-xkb
               #:cl-wayland)
  :serial t
  :components ((:file "backend")
	       (:file "animation")
	       (:file "package")
	       (:file "client")
	       (:file "compositor")
	       (:file "render")
	       (:file "mode")
	       (:file "default-mode")
	       (:file "alt-tab-mode")
	       ;;(:file "backend-sdl/backend-sdl")
	       (:file "plumbing")
               (:file "ulubis")))

