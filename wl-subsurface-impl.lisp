
(in-package :ulubis)

(def-wl-callback set-position (client subsurface (x :int) (y :int))
  (when subsurface
    (setf (x subsurface) x)
    (setf (y subsurface) y)))

(def-wl-callback subsurface-destroy (client subsurface)
  (with-slots (parent) subsurface
    (setf (subsurfaces parent) (remove subsurface (subsurfaces parent)))))

(defimplementation wl-subsurface (isurface ianimatable)
  ((:set-position set-position)
   (:destroy subsurface-destroy))
  ((parent :accessor parent :initarg :parent :initform nil)))
