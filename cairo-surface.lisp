
(in-package :ulubis)

(defclass cairo-surface ()
  ((surface :reader surface :initform nil)
   (context :reader context :initform nil)
   (gl-texture :reader gl-texture :initform nil)
   (width :reader width :initarg :width :initform 64)
   (height :reader height :initarg :height :initform 64)
   (allow-gl :reader allow-gl :initarg :allow-gl :initform nil)
   (gl-texture-up-to-date :initform nil)
   (draw-func :accessor draw-func :initarg :draw-func :initform (lambda ()))))

(defmethod initialize-instance :after ((instance cairo-surface) &key)
  (with-slots (surface context) instance
    (setf surface
          (cl-cairo2:create-image-surface :argb32 (width instance) (height instance)))
    (setf context
          (cl-cairo2:create-context surface)))
  (trivial-garbage:finalize instance #'finalize-instance))

(defgeneric finalize-instance (instance))
(defmethod finalize-instance ((instance cairo-surface))
  (with-slots (surface gl-texture) instance
    (when gl-texture
      (cepl:free gl-texture))))

(defgeneric cairo-surface-redraw (instance &optional custom-draw-func)
  (:documentation "Calls DRAW-FUNC to update surface pixels.
The call itself doesn't upload pixels to GPU, so it can be safely
called more often than CAIRO-SURFACE->GL-TEXTURE"))

(defmethod cairo-surface-redraw ((instance cairo-surface) &optional custom-draw-func)
  (with-slots (surface context gl-texture-up-to-date) instance
    (let ((cl-cairo2:*surface* surface)
          (cl-cairo2:*context* context))
      (if custom-draw-func
          (funcall custom-draw-func)
          (funcall (draw-func instance))))
    (setf gl-texture-up-to-date nil)))

(defgeneric cairo-surface->gl-texture (instance)
  (:documentation "Uploads the new data to GPU if cairo surface was
updated after the previous call. Returns CEPL texture object."))

(defmethod cairo-surface->gl-texture ((instance cairo-surface))
  (unless (allow-gl instance)
    (error "This cairo surface isn't set up to upload pixels to GPU.~%Must create it with :allow-gl t"))
  (with-slots (surface gl-texture gl-texture-up-to-date width height) instance
    (unless gl-texture-up-to-date
      (cl-cairo2:surface-flush surface)
      (let* ((cairo-data (cl-cairo2:image-surface-get-data surface :pointer-only t))
             (cepl-data (cepl:make-c-array-from-pointer (list width height)
                                                        :uint8-vec4
                                                        cairo-data)))
        (if gl-texture
            (cepl:push-g cepl-data gl-texture)
            (setf gl-texture (cepl:make-texture cepl-data)))
        (setf gl-texture-up-to-date t)))
    gl-texture))

