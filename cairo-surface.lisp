
(in-package :ulubis)

(defclass cairo-surface ()
  ((surface :reader surface :initform nil)
   (context :reader context :initform nil)
   (gl-texture :reader gl-texture :initform nil)
   (allow-gl :reader allow-gl :initarg :allow-gl :initform nil)
   (gl-texture-up-to-date :initform nil)
   (draw-func :accessor draw-func :initarg :draw-func :initform (lambda ()))))

(defmethod initialize-instance :after ((instance cairo-surface) &key width height filename)
  (with-slots (surface context) instance
    (if filename
        (if (and width height)
            (error "Need to specify either :filename or :width and :height")
            (setf surface (cl-cairo2:image-surface-create-from-png filename)))
        (if (and width height)
            (setf surface (cl-cairo2:create-image-surface :argb32 width height))
            (error "Need to specify either :filename or :width and :height")))
    (setf context (cl-cairo2:create-context surface)))
  (trivial-garbage:finalize instance #'finalize-instance))

(defgeneric finalize-instance (instance))
(defmethod finalize-instance ((instance cairo-surface))
  (with-slots (surface gl-texture) instance
    (when gl-texture
      (cepl:free gl-texture))))

(defmethod width ((instance cairo-surface))
  (cl-cairo2:width (surface instance)))

(defmethod height ((instance cairo-surface))
  (cl-cairo2:height (surface instance)))

(defgeneric cairo-surface-redraw (instance &optional custom-draw-func)
  (:documentation "Calls DRAW-FUNC to update surface pixels.
The call itself doesn't upload pixels to GPU, so it can be safely
called more often than CAIRO-SURFACE->GL-TEXTURE"))

(defmethod cairo-surface-redraw ((instance cairo-surface) &optional custom-draw-func)
  (with-slots (surface context gl-texture-up-to-date) instance
    (let ((cl-cairo2:*surface* surface)
          (cl-cairo2:*context* context))
      (cl-cairo2:reset-trans-matrix)
      (if custom-draw-func
          (funcall custom-draw-func)
          (funcall (draw-func instance))))
    (setf gl-texture-up-to-date nil)))

(defmethod texture-of ((instance cairo-surface))
  (unless (allow-gl instance)
    (error "This cairo surface isn't set up to upload pixels to GPU.~%Must create it with :allow-gl t"))
  (with-slots (surface gl-texture gl-texture-up-to-date) instance
    (unless gl-texture-up-to-date
      (cl-cairo2:surface-flush surface)
      (let* ((cairo-data (cl-cairo2:image-surface-get-data surface :pointer-only t))
             (cepl-data (cepl:make-c-array-from-pointer (list (width instance)
                                                              (height instance))
                                                        :uint8-vec4
                                                        cairo-data)))
        (if gl-texture
            (cepl:push-g cepl-data gl-texture)
            (setf gl-texture (cepl:make-texture cepl-data
                                                :pixel-format (cepl.types::make-pixel-format
                                                               :components :bgra
                                                               :type :uint8
                                                               :normalize t
                                                               :sizes nil
                                                               :reversed t
                                                               :comp-length 4))))
        (setf gl-texture-up-to-date t)))
    (cepl:sample gl-texture)))

