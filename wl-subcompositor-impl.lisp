
(in-package :ulubis)

(def-wl-callback get-subsurface (client subcompositor (id :uint32) (surface-ptr :pointer) (parent-ptr :pointer))
  (let* ((subsurface (make-wl-subsurface (->client client) (get-version subcompositor) id))
	 (surface (find-resource client surface-ptr))
	 (parent (find-resource client parent-ptr)))
    (setf (parent subsurface) parent)
    (setf (wl-surface subsurface) surface)
    (setf (role surface) subsurface)
    (push subsurface (subsurfaces (role parent)))))

(defimplementation wl-subcompositor ()
  ((:get-subsurface get-subsurface))
  ())

#|
(def-wl-bind subcompositor-bind (client (data :pointer) (version :uint32) (id :uint32))
  (let ((sbcmp (make-wl-subcompositor client 1 id)))
    (format t "Made wl-subcompositor: ~A~%" sbcmp)))
|#

(defcallback subcompositor-bind :void ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
  (bind-wl-subcompositor client-ptr 1 id))

#|
(defcallback subcompositor-bind :void
    ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
  (format t "subcompositor-bind called~%")
  (let ((subcompositor (wl-resource-create client-ptr wl-subcompositor-interface 1 id)))
    (wl-resource-set-implementation
     subcompositor
     wl-subcompositor-implementation
     (null-pointer)
     (null-pointer))))
|#
