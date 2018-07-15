
(in-package :ulubis)

(def-wl-callback set-size (client zxdg-positioner (width :uint32) (height :uint32))
  (format t "set-size: ~A ~A~%" width height))

(def-wl-callback set-anchor-rect (client zxdg-positioner (x :uint32) (y :uint32) (width :uint32) (height :uint32))
  (format t "set-anchor-rect: ~A ~A ~A ~A~%" x y width height))

(def-wl-callback set-anchor (client zxdg-positioner (anchor :uint32))
  (format t "set-anchor: ~A~%" anchor))

(def-wl-callback set-gravity (client zxdg-positioner (gravity :uint32))
  (format t "set-gravity: ~A~%" gravity))

(def-wl-callback set-constraint-adjustment (client zxdg-positioner (constraint-adjustment :uint32))
  (format t "set-constraint-adjustment: ~A~%" constraint-adjustment))

(def-wl-callback set-offset (client zxdg-positioner (x :uint32) (y :uint32))
  (format t "set-offset: ~A ~A~%" x y))

(defimplementation zxdg-positioner-v6 ()
  ((:set-size set-size)
   (:set-anchor-rect set-anchor-rect)
   (:set-anchor set-anchor)
   (:set-gravity set-gravity)
   (:set-constraint-adjustment set-constraint-adjustment)
   (:set-offset set-offset))
  ())
