
(in-package :ulubis)

(def-wl-callback create-surface (client compositor (id :uint32))
  (let ((surface (make-wl-surface client 3 id)))
    (setf (wl-surface surface) surface)
    (setf (role surface) surface)))

(def-wl-callback create-region (client compositor (id :uint32))
  (make-wl-region client 1 id))

(defimplementation wl-compositor ()
  ((:create-surface create-surface)
   (:create-region create-region))
  ())

(def-wl-bind compositor-bind (client (data :pointer) (version :uint32) (id :uint32))
  (make-wl-compositor client 1 id))
