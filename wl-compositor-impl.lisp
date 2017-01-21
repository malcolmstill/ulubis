
(in-package :ulubis)

(def-wl-callback create-surface (client compositor (id :uint32))
  (let ((surface (make-wl-surface (->client client) 3 id)))
    (setf (wl-surface surface) surface)
    (setf (role surface) surface)))

(def-wl-callback create-region (client compositor (id :uint32))
  (make-wl-region (->client client) 1 id))

(defimplementation wl-compositor ()
  ((:create-surface create-surface)
   (:create-region create-region))
  ())

#|
(def-wl-bind compositor-bind (client (data :pointer) (version :uint32) (id :uint32))
  (let ((compositor (make-wl-compositor client 1 id)))
    (format t "Made compositor: ~A~%" compositor)))
|#

(defcallback compositor-bind :void ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
  ;;(make-wl-compositor client-ptr 1 id))
  (bind-wl-compositor client-ptr 1 id))

#|
(defcallback compositor-bind :void
    ((client :pointer) (data :pointer) (version :uint32) (id :uint32))
  (format t "Binding compositor~%")
  (let ((resource (wl-resource-create client wl-compositor-interface 1 id)))
    (wl-resource-set-implementation resource wl-compositor-implementation (null-pointer) (null-pointer))
    ))
|#
