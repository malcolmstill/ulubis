
(in-package :ulubis)

(defparameter *data-sources* nil)
(defparameter *data-devices* nil)

(def-wl-callback create-data-source (client resource (id :uint32))
  (let ((data-source (make-wl-data-source client (get-version resource) id)))
    (push data-source *data-sources*)))

(def-wl-callback get-data-device (client resource (id :uint32))
  (let ((data-device (make-wl-data-device client (get-version resource) id)))
    (push data-device *data-devices*)))

(defimplementation wl-data-device-manager ()
  ((:create-data-source create-data-source)
   (:get-data-device get-data-device))
  ())

(def-wl-bind device-manager-bind (client (data :pointer) (version :uint32) (id :uint32))
  (make-wl-data-device-manager client 1 id))
