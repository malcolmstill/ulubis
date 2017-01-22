
(in-package :ulubis)

(def-wl-callback xdg-shell-get-xdg-surface (client xdg-shell (id :uint32) (surface-ptr :pointer))
  (let* ((surface (find-resource client surface-ptr))
	 (xdg-surface (make-xdg-surface client 1 id)))
    (setf (wl-surface xdg-surface) surface)
    (setf (role surface) xdg-surface)
    (push xdg-surface (surfaces (current-view *compositor*)))))

(defimplementation xdg-shell ()
  ((:get-xdg-surface xdg-shell-get-xdg-surface))
  ())

#|
(defcallback client-delete :void ((client-ptr :pointer))
	     (remove-client (get-client client-ptr)))
|#
(def-wl-delete client-delete (xdg-shell)
  (when xdg-shell
    (format t "DELETING CLIENT~%")
    (remove-client (->client (client xdg-shell)))
    (setf (render-needed *compositor*) t))
  )

(def-wl-bind xdg-shell-bind (client (data :pointer) (version :uint32) (id :uint32))
  (let ((shell (make-xdg-shell client 1 id :delete-fn (callback client-delete))))
    (format t "Made zxdg-shell: ~A~%" shell)))
