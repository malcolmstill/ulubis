
(in-package :ulubis)

(defimplementation xdg-surface (isurface ianimatable)
  ((:move move)) ;; Same as zxdg-shell-v6
  ())

(defmethod activate ((surface xdg-surface) active-surface mods)
  (call-next-method)
  (with-wl-array array
    (setf (mem-aref (wl-array-add array 4) :int32) 4)
    (xdg-surface-send-configure (->resource surface) 0 0 array (get-milliseconds)))
  surface)

(defmethod deactivate ((surface xdg-surface))
  (call-next-method)
  (with-wl-array array
    (xdg-surface-send-configure (->resource surface) 0 0 array (get-milliseconds))))

