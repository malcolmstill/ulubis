
(in-package :ulubis)

(def-wl-callback get-xdg-surface (client zxdg-shell (id :uint32) (surface-ptr :pointer))
  (let* ((surface (find-resource client surface-ptr))
	 (zxdg-surface (make-zxdg-surface-v6 client 1 id)))
    (setf (wl-surface zxdg-surface) surface)
    (setf (role surface) zxdg-surface)))

(defimplementation zxdg-shell-v6 ()
  ((:get-xdg-surface get-xdg-surface))
  ())

(def-wl-delete client-delete (zxdg-shell)
  (when zxdg-shell
    (remove-client (->client (client zxdg-shell)))
    (setf (render-needed *compositor*) t)))

(def-wl-bind zxdg-shell-v6-bind (client (data :pointer) (version :uint32) (id :uint32))
  (make-zxdg-shell-v6 client 1 id :delete-fn (callback client-delete)))
