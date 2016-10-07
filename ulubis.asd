;;;; ulubis.asd

(asdf:defsystem #:ulubis
  :description "Describe ulubis here"
  :author "Malcolm Still"
  :license "Specify license here"
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

