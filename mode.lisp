;; Here we define a mode system
;; Modes encapsulate a behaviour
;; I.e. we can set a custom rendering function,
;; custom shortcuts, etc.

(in-package :ulubis)

(defclass mode ()
  ((blending-parameters :accessor blending-parameters :initarg :blending-parameters :initform nil)))

(defgeneric init-mode (mode))
(defgeneric mouse-motion-handler (mode x y))
(defgeneric mouse-button-handler (mode button state))
(defgeneric keyboard-handler (mode key state mods))
(defgeneric render (mode))
(defgeneric first-commit (mode surface))

(defmethod init-mode :before ((mode mode))
  (setf (blending-parameters mode) (cepl:make-blending-params)))

(defmethod first-commit ((mode mode) surface)
  )

(defmethod first-commit :after ((mode mode) surface)
  (setf (first-commit? surface) nil))

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
