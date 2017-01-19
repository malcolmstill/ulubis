
(in-package :ulubis)

(def-wl-callback compositor-create-surface (client resource (id :uint32)) 
  (let ((surface (make-wl-surface client 3 id)))
    (push surface (surfaces *compositor*))
    (setf (wl-surface surface) surface)))

(def-wl-callback compositor-create-region (client resource (id :uint32)) 
  (make-wl-region client 1 id))

(defimplementation wl-compositor ()
  ((:create-surface compositor-create-surface)
   (:create-region compositor-create-region))
  ())

(def-wl-bind compositor-bind (client (data :pointer) (version :uint32) (id :uint32))
  (make-wl-compositor client 1 id))
   
