
(in-package :ulubis)

(def-wl-callback get-pointer (client seat (id :uint32))
  (setf (pointer client) (make-wl-pointer client 1 id)))

(def-wl-callback get-keyboard (client seat (id :uint32))
  (let ((keyboard (make-wl-keyboard client (get-version seat) id)))
    (setf (keyboard client) keyboard)
    (when (>= (get-version keyboard) 4)
      (wl-keyboard-send-repeat-info (->resource keyboard) 30 200)
      (multiple-value-bind (fd size) (get-keymap *compositor*)
	(wl-keyboard-send-keymap (->resource keyboard) 1 fd size)))))

(defimplementation wl-seat ()
  ((:get-keyboard get-keyboard)
   (:get-pointer get-pointer))
  ())

(def-wl-bind seat-bind (client (data :pointer) (version :uint32) (id :uint32))
  (let ((seat (make-wl-seat client 4 id)))
    (format t "Made seat: ~A~%" seat)
    (wl-seat-send-capabilities (->resource seat) 3)))

#|
(defcallback seat-bind :void ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
  (let ((seat (bind-wl-seat client-ptr 4 id)))
    (wl-seat-send-capabilities seat 3)))
|#

#|
(defcallback seat-bind :void
    ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
    (format t "seat-bind called ~A~%" client-ptr)
  (let ((seat (wl-resource-create client-ptr wl-seat-interface 4 id)))
    (wl-resource-set-implementation
     seat
     wl-seat-implementation
     (null-pointer)
     (null-pointer))
    (wl-seat-send-capabilities seat 3))) ;; WL_SEAT_CAPABILITY_POINTER | WL_SEAT_CAPABILITY_KEYBOARD
|#
