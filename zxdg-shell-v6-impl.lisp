
(in-package :ulubis)

(def-wl-callback get-xdg-surface (client resource (id :uint32) (surface :pointer))
  (let* ((surface (find-resource client (wl-resource-get-user-data surface)))
	 (zxdg-surface (make-zxdg-surface-v6 client (get-version surface) id)))
    (setf (wl-surface zxdg-surface) surface)
    (setf (role surface) zxdg-surface)))

(defimplementation zxdg-shell-v6 ()
  ((:get-xdg-surface get-xdg-surface))
  ())

(def-wl-bind zxdg-shell-v6-bind (client (data :pointer) (version :uint32) (id :uint32))
  (make-zxdg-shell-v6 client 1 id))
