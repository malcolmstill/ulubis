;; Here we define a mode system
;; Modes encapsulate a behaviour
;; I.e. we can set a custom rendering function,
;; custom shortcuts, etc.

(in-package :ulubis)

(defclass mode ()
  ((blending-parameters :accessor blending-parameters :initarg :blending-parameters :initform nil)))

;; We introduce defmode. Ideally we would just subclass mode,
;; but we'd like to a class allocated slot KEY-BINDINGS. We
;; can't inherit this from MODE because the subclasses would
;; share that allocation. Instead we introduce this macro
;; to include a subclass-specific KEY-BINDINGS.
(defmacro defmode (name (&rest superclasses) (&body slots))
  "DEFMODE automatically inherits MODE and provides class-allocated slot KEY-BINDINGS"
  `(defclass ,name (,@superclasses mode)
     ((key-bindings :accessor key-bindings :initarg :key-bindings :initform nil :allocation :class)
      (view :accessor view :initarg :view :initform nil)
      ,@slots)))

(defgeneric init-mode (mode))
(defgeneric mouse-motion-handler (mode time delta-x delta-y))
(defgeneric mouse-button-handler (mode time button state))
(defgeneric keyboard-handler (mode time key state))
(defgeneric render (mode &optional view-fbo))
(defgeneric first-commit (mode surface))

(defmethod init-mode :before ((mode mode))
  (setf (blending-parameters mode) (cepl:make-blending-params)))

(defclass key-binding ()
  ((op :accessor op :initarg :op :initform :pressed)
   (key :accessor key :initarg :key :initform nil)
   (mods :accessor mods :initarg :mods :initform nil)
   (fn :accessor fn :initarg :fn :initform (lambda ()))))

(defconstant Shift 1)
(defconstant Ctrl 4)
(defconstant Alt 8)
(defconstant Gui 64)

;; We create a dummy instance of each mode. Not pretty
;; but alternatively we can use mop:class-prototype
;; to get access to a non-consed class-allocated slot
(defmacro defkeybinding ((op key &rest mods) (mode-ref) modes &body body)
  (let ((mode (gensym "mode"))
	(instance (gensym "instance")))
    `(loop :for ,mode :in ',modes :do
	(let ((,instance (make-instance ,mode)))
	  (push (make-instance 'key-binding
			       :op ,op
			       :key ,key
			       :mods (logior ,@mods)
			       :fn (lambda (,mode-ref)
				     ,@body))
		(key-bindings ,instance))))))

(defun cancel-mods (surface)
  (when (and surface (->keyboard (client surface)))
    (wl-keyboard-send-modifiers (->keyboard (client surface)) 0
				0
				0
				0
				0)))

(defmethod keyboard-handler (mode time akey state)
  (let ((surface (active-surface (view mode))))
    (loop :for key-binding :in (key-bindings mode) :do
       (with-slots (op key mods fn) key-binding
	 (when (and (eq op :pressed)
		    (or (not akey) (= akey key))
		    (= 1 state)
		    (or (zerop mods) (= (mods-depressed *compositor*) mods)))
	   (format t "Calling pressed~%")
	   (cancel-mods surface)
	   (funcall fn mode)
	   (return-from keyboard-handler))
	 (when (and (eq op :released)
		    (= 0 state)
		    (or (not key) (and akey (= akey key) (= state 0)))
		    (zerop (logand (mods-depressed *compositor*) mods)))
	   (format t "Calling released~%")
	   (cancel-mods surface)
	   (funcall fn mode)
	   (return-from keyboard-handler))))
    (when (and surface akey (->keyboard (client surface)))
      (wl-keyboard-send-key (->keyboard (client surface)) 0 time akey state))
    (when (and surface (->keyboard (client surface)))
      (wl-keyboard-send-modifiers (->keyboard (client surface)) 0
				  (mods-depressed *compositor*)
				  (mods-latched *compositor*)
				  (mods-locked *compositor*)
				  (mods-group *compositor*)))))

(defmethod first-commit ((mode mode) surface)
  )

(defmethod first-commit :after ((mode mode) surface)
  (setf (first-commit? surface) nil))

(defun push-mode (view mode)
  (setf (view mode) view)
  (init-mode mode)
  (push mode (modes view))
  (setf (render-needed *compositor*) t))
  
(defun pop-mode (mode)
  (with-slots (view) mode
    (pop (modes view))))
