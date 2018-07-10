
(in-package :ulubis)

(def-wl-callback get-shell-surface (client shell (id :uint32) (surface-ptr :pointer))
  (let ((surface (find-resource client surface-ptr))
	(shell-surface (make-wl-shell-surface client 1 id :delete-fn (callback wl-shell-surface-delete))))
    (setf (wl-surface shell-surface) surface)
    (setf (role surface) shell-surface)
    (push shell-surface (surfaces (active-surface (screen *compositor*))))))

(def-wl-delete wl-shell-surface-delete (shell-surface)
  (remove-surface shell-surface *compositor*)
  (setf (render-needed *compositor*) t))

(defimplementation wl-shell ()
  ((:get-shell-surface get-shell-surface))
  ())

(def-wl-bind shell-bind (client (data :pointer) (version :uint32) (id :uint32))
  (make-wl-shell client 1 id))
