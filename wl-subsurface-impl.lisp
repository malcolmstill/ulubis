
(in-package :ulubis)

(def-wl-callback set-position (client subsurface (x :int) (y :int))
  (when subsurface
    (setf (x subsurface) x)
    (setf (y subsurface) y)))

(defimplementation wl-subsurface (isurface ianimatable)
  ((:set-position set-position))
  ((parent :accessor parent :initarg :parent :initform nil)))
