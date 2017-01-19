
(in-package :ulubis)

(def-wl-callback get-subsurface (client resource (id :uint32) (surface-ptr :pointer) (parent-ptr :pointer))
  (let* ((subsurface (make-wl-subsurface client (wl-resource-get-version resource) id))
	 (surface (find-resource client surface-ptr))
	 (parent (find-resource client pointer-ptr)))
    (setf (parent subsurface) parent)
    (setf (surface subsurface) surface)))

(defimplementation wl-subcompositor ()
  ((:get-subsurface get-subsurface))
  ())

(def-wl-bind subcompositor-bind (client (data :pointer) (version :uint32) (id :uint32))
  (make-wl-subcompositor client 1 id))
