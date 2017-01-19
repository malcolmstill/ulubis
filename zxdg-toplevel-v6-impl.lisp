
(in-package :ulubis)

(def-wl-callback set-title (client toplevel (title :string))
  (format t "Setting title of ~A to ~A~%" toplevel title))

(def-wl-callback move (client toplevel (seat :pointer) (serial :uint32))
  )

(def-wl-callback zxdg-toplevel-destroy (client toplevel)
  (remove-surface toplevel *compositor*)
  (setf (render-needed *compositor*) t))

(def-wl-delete zxdg-toplevel-delete (toplevel)
  (remove-surface toplevel *compositor*)
  (setf (render-needed *compositor*) t))

(defimplementation zxdg-toplevel-v6 (isurface)
  ((:move move)
   (:destroy zxdg-toplevel-destroy)
   (:set-title set-title))
  ((zxdg-surface-v6 :accessor zxdg-surface-v6 :initarg :zxdg-surface-v6 :initform nil)))

(defmethod activate ((surface zxdg-toplevel-v6) active-surface mods)
  (call-next-method)
  (with-wl-array array
    (setf (mem-aref (wl-array-add array 4) :int32) 4)
    (zxdg-toplevel-v6-send-configure (->resource surface) 0 0 array)
    (zxdg-surface-v6-send-configure (->resource (zxdg-surface-v6 surface)) 0))
  surface)

(defmethod deactivate ((surface zxdg-toplevel-v6))
  (call-next-method)
  (with-wl-array array
    (zxdg-toplevel-v6-send-configure (->resource surface) 0 0 array)
    (zxdg-surface-v6-send-configure (->resource (zxdg-surface-v6 surface)) 0)))


