;; Here we define a mode system
;; Modes encapsulate a behaviour
;; I.e. we can set a custom rendering function,
;; custom shortcuts, etc.

(in-package :ulubis)

(defclass mode ()
  ((blending-parameters :accessor blending-parameters :initarg :blending-parameters :initform nil)))

(defmethod init-mode :before ((mode mode))
  (setf (blending-parameters mode) (cepl:make-blending-params)))

(defun current-mode ()
  (let ((mode (first (modes *compositor*))))
    (if mode
	mode
	*default-mode*)))

(defun push-mode (mode)
  (init-mode mode)
  (push mode (modes *compositor*))
  (setf (render-needed *compositor*) t))
  
(defun pop-mode ()
  (pop (modes *compositor*)))  
