
(in-package :ulubis)

(def-wl-callback create-surface (client compositor (id :uint32))
  (format t "CREATE-SURFACE: ~A~%" compositor)
  (let ((surface (make-wl-surface client 3 id)))
    (format t "Made surface: ~A~%" surface)
    (setf (wl-surface surface) surface)
    (setf (role surface) surface)))

(def-wl-callback create-region (client compositor (id :uint32))
  (format t "CREATE-REGION: ~A~%" compositor)
  (let ((reg (make-wl-region client 1 id)))
    (format t "Made region: ~A~%" reg)))

(defimplementation wl-compositor ()
  ((:create-surface create-surface)
   (:create-region create-region))
  ())

(def-wl-bind compositor-bind (client (data :pointer) (version :uint32) (id :uint32))
  (let ((compositor (make-wl-compositor client 1 id)))
    (format t "Made compositor: ~A~%" compositor)))
   
