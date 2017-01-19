
(in-package :ulubis)

(def-wl-callback set-position (client surface (x :int) (y :int))
  (when surface
    (setf (x surface) x)
    (setf (y surface) y)))

(defimplementation wl-subsurface (isurface)
  ((:set-position set-position))
  ())
