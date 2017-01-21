
(in-package :ulubis)

(def-wl-callback get-shell-surface (client shell (id :uint32) (surface-ptr :pointer))
  (let ((surface (find-resource client surface-ptr))
	(shell-surface (make-wl-shell-surface (->client client) 1 id :delete-fn (callback wl-shell-surface-delete))))
    (setf (wl-surface shell-surface) surface)
    (setf (role surface) shell-surface)
    (push shell-surface (surfaces (current-view *compositor*)))))

(def-wl-delete wl-shell-surface-delete (shell-surface)
  (remove-surface shell-surface *compositor*)
  (setf (render-needed *compositor*) t))

(defimplementation wl-shell ()
  ((:get-shell-surface get-shell-surface))
  ())

#|
(def-wl-bind shell-bind (client (data :pointer) (version :uint32) (id :uint32))
  (make-wl-shell client 1 id))
|#

(defcallback shell-bind :void ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
  (bind-wl-shell client-ptr 1 id))

#|
(defcallback shell-bind :void
  ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
  (format t "~%shell-bind called: ~A~%" client-ptr)
  (waylisp:get-client client-ptr)
  (wl-resource-set-implementation
   (wl-resource-create client-ptr wl-shell-interface 1 id)
   wl-shell-implementation
   client-ptr
   (null-pointer)));;(callback client-destroy)))
|#
