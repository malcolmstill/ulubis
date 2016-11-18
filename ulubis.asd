;;;; ulubis.asd

(asdf:defsystem #:ulubis
  :description "A Common Lisp Wayland compositor"
  :author "Malcolm Still"
  :license "BSD 3-Clause"
  :depends-on (#:cffi
	       #:osicat
	       #:swank
	       #:cepl
	       #:easing
	       #:cl-xkb
               #:cl-wayland
	       #:trivial-dump-core
	       #:uiop)
  :serial t
  :components ((:file "backend")
	       (:file "syscall")
	       (:file "animation")
	       (:file "package")
	       (:file "client")
	       (:file "compositor")
	       (:file "render")
	       (:file "mode")
	       (:file "view")
	       (:file "desktop-mode")
	       (:file "alt-tab-mode")
	       (:file "plumbing")
               (:file "ulubis")
	       (:file "install")))

