
(in-package :ulubis)

(def-wl-callback grab (client zxdg-popup (seat :pointer) (serial :uint32))
  (format t "grab: ~A ~A~%" seat serial))

(def-wl-delete zxdg-popup-delete (popup)
  (when popup
    (setf (role (wl-surface popup)) nil)
    (remove-surface popup *compositor*)
    (setf (render-needed *compositor*) t)))

(def-wl-callback zxdg-popup-destroy (client popup)
  (when popup
    (setf (role (wl-surface popup)) nil)
    (remove-surface popup *compositor*)
    (setf (render-needed *compositor*) t)))

(defimplementation zxdg-popup-v6 (isurface ianimatable)
  ((:grab grab)
   (:destroy zxdg-popup-destroy))
  ((zxdg-surface-v6 :accessor zxdg-surface-v6
		    :initarg :zxdg-surface-v6
		    :initform nil)))
