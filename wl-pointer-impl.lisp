
(in-package :ulubis)

(def-wl-callback set-cursor (client pointer (serial :uint32) (surface-ptr :pointer) (hotspot-x :int32) (hotspot-y :int32))
  (let ((surface (find-resource client surface-ptr)))
    (when surface
      (setf (x surface) hotspot-x)
      (setf (y surface) hotspot-y)
      (when (first-commit-animation surface)
	(stop-animation (first-commit-animation surface)))
      (setf (cursor-surface *compositor*) surface))))

(defimplementation wl-pointer ()
  ((:set-cursor set-cursor))
  ())
