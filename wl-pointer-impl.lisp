
(in-package :ulubis)

(def-wl-callback set-cursor (client resource (serial :uint32) (->surface :pointer) (hotspot-x :int32) (hotspot-y :int32))
  (let ((surface (find-resource client ->surface)))
    (when surface
      (setf (x surface) hotspot-x)
      (setf (y surface) hotspot-y)
      (setf (cursor-surface *compositor*) surface))))

(defimplementation wl-pointer ()
  ((:set-cursor set-cursor))
  ())
