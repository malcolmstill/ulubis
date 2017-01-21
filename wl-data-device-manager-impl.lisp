
(in-package :ulubis)

(defparameter *data-sources* nil)
(defparameter *data-devices* nil)

(def-wl-callback create-data-source (client data-device-manager (id :uint32))
  (let ((data-source (make-wl-data-source client (get-version data-device-manager) id)))
    (push data-source *data-sources*)))

(def-wl-callback get-data-device (client data-device-manager (id :uint32))
  (let ((data-device (make-wl-data-device client (get-version data-device-manager) id)))
    (push data-device *data-devices*)))

(defimplementation wl-data-device-manager ()
  ((:create-data-source create-data-source)
   (:get-data-device get-data-device))
  ())

(def-wl-bind device-manager-bind (client (data :pointer) (version :uint32) (id :uint32))
  (let ((ddm (make-wl-data-device-manager client 1 id)))
    (format t "Made data-device-manager: ~A~%" ddm)))

#|
(defcallback device-manager-bind :void ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
  (bind-wl-data-device-manager client-ptr 1 id))
|#

#|
(defcallback device-manager-bind :void
    ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
  (format t "Device manager bind~%")
  (let ((device-manager (wl-resource-create client-ptr wl-data-device-manager-interface 1 id)))
    (wl-resource-set-implementation
     device-manager  
     wl-data-device-manager-implementation
     (null-pointer)
     (null-pointer))))
|#
