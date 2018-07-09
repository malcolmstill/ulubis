
(in-package :ulubis)

(def-wl-callback set-title (client toplevel (title :string))
  (format t "Setting title of ~A to ~A~%" toplevel title))

(def-wl-callback move (client toplevel (seat :pointer) (serial :uint32))
  (setf (moving-surface *compositor*) (make-move-op :surface toplevel
						    :surface-x (x toplevel)
						    :surface-y (y toplevel)
						    :pointer-x (pointer-x *compositor*)
						    :pointer-y (pointer-y *compositor*))))

(def-wl-callback zxdg-toplevel-destroy (client toplevel)
  (setf (role (wl-surface toplevel)) nil)
  (remove-surface toplevel *compositor*)
  (setf (render-needed *compositor*) t))

(def-wl-delete zxdg-toplevel-delete (toplevel)
  (when toplevel
    (setf (role (wl-surface toplevel)) nil)
    (remove-surface toplevel *compositor*)
    (setf (render-needed *compositor*) t)))

(defimplementation zxdg-toplevel-v6 (isurface ianimatable)
  ((:move move)
   (:destroy zxdg-toplevel-destroy)
   (:set-title set-title))
  ((zxdg-surface-v6 :accessor zxdg-surface-v6
		    :initarg :zxdg-surface-v6
		    :initform nil)))

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

(defmethod resize ((surface zxdg-toplevel-v6) width height time &key (activate? t))
  (with-wl-array array
    (setf (mem-aref (wl-array-add array 4) :int32) 3)
    (when activate?
      (setf (mem-aref (wl-array-add array 4) :int32) 4))
    (zxdg-toplevel-v6-send-configure (->resource surface) (round width) (round height) array)
    (zxdg-surface-v6-send-configure (->resource (zxdg-surface-v6 surface)) 0)))
