
(in-package :ulubis)

(defimplementation wl-output ()
  ()
  ())

(def-wl-bind output-bind (client (data :pointer) (version :uint32) (id :uint32))
  (let ((output (make-wl-output client 1 id :implementation? nil)))
    (wl-output-send-geometry (->resource output) 0 0 1440 900 0 "apple" "apple" 0)))
