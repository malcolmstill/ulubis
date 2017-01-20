
(in-package :ulubis)

(def-wl-callback get-xdg-surface (client zxdg-shell (id :uint32) (surface-ptr :pointer))
  (let* ((surface (find-resource client surface-ptr))
	 (zxdg-surface (make-zxdg-surface-v6 client (get-version zxdg-shell) id)))
    (format t "GET-XDG-SURFACE: ~A~%" zxdg-shell)
    (format t "GET-XDG-SURFACE surface: ~A~%" surface)
    (format t "GET-XDG-SURFACE zxdg-surface: ~A~%" zxdg-surface)
    (setf (wl-surface zxdg-surface) surface)
    (setf (role surface) zxdg-surface)
    (describe surface)))

(defimplementation zxdg-shell-v6 ()
  ((:get-xdg-surface get-xdg-surface))
  ())

#|
(defcallback client-delete :void ((client-ptr :pointer))
	     (remove-client (get-client client-ptr)))
|#
(def-wl-delete client-delete (zxdg-shell)
  (format t "DELETING CLIENT~%")
  (remove-client (->client (client zxdg-shell)))
  (setf (render-needed *compositor*) t)
  )

(def-wl-bind zxdg-shell-v6-bind (client (data :pointer) (version :uint32) (id :uint32))
  (let ((shell (make-zxdg-shell-v6 client 1 id :delete-fn (callback client-delete))))
    (format t "Made zxdg-shell: ~A~%" shell)))
