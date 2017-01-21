
(in-package :ulubis)

(defimplementation wl-output ()
  ()
  ())

#|
(def-wl-bind output-bind (client (data :pointer) (version :uint32) (id :uint32))
  (let ((output (make-wl-output client 1 id :implementation? nil)))
    ;;(setf (->output *compositor*) output)
    (format t "Made output: ~A~%" output)
    (wl-output-send-geometry (->resource output) 0 0 1440 900 0 "apple" "apple" 0)))
|#

(defcallback output-bind :void ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
  (let ((output (bind-wl-output client-ptr 1 id :implementation? nil)))
    (wl-output-send-geometry output 0 0 1440 900 0 "apple" "apple" 0)))

#|
(defcallback output-bind :void
    ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
  (let ((output (wl-resource-create client-ptr wl-output-interface 1 id)))
    (setf (->output *compositor*) output)))
|#
