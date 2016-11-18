
(in-package :ulubis)

(defclass texture-gl ()
  ((width :accessor width :initarg :width :initform 0)
   (height :accessor height :initarg :height :initform 0)
   (cepl-texture :accessor cepl-texture :initarg :cepl-texture :initform 0)))

(defun create-texture (surface)
  (when (not (pointer-eq (->buffer surface) (null-pointer)))
    (let* ((buffer (->buffer surface))
	   (shm-buffer (wl-shm-buffer-get buffer))
	   (w (wl-shm-buffer-get-width shm-buffer))
	   (h (wl-shm-buffer-get-height shm-buffer))
	   (stride (wl-shm-buffer-get-stride shm-buffer))
	   (array (make-c-array-from-pointer
		   (list w h)
		   :uint8-vec4 
		   (wl-shm-buffer-get-data shm-buffer))))
      (setf (width surface) w)
      (setf (height surface) h)
      (when (and (texture surface) (cepl-texture (texture surface)))
	(free (cepl-texture (texture surface))))
      (setf (texture surface) (make-instance 'texture-gl
					     :width w
					     :height h
					     :cepl-texture
					     (make-texture
					      array
					      :element-type :rgba8)))
      ;; Copy pixels from shared-memory buffer to SDL texture
      (wl-buffer-send-release buffer)
      (setf (->buffer surface) (null-pointer)))))

(defmacro with-blending-on-fbo (blending-params fbo &body body)
  (let ((b-params (gensym "blending-params")))
    `(let* ((,b-params ,blending-params))
       (cepl.blending::%with-blending ,fbo nil ,b-params
         ,@body))))

(defmacro with-screen ((vertex-stream) &body body)
  (let ((result (gensym "result"))
	 (array (gensym "array")))
    `(let* ((,array (make-gpu-array (list (list (v! -1 -1 0)
						(v! 0 0))
					  (list (v! -1 1 0)
						(v! 0 1))
					  (list (v! 1 1 0)
						(v! 1 1))
					  (list (v! 1 1 0)
						(v! 1 1))
					  (list (v! 1 -1 0)
						(v! 1 0))
					  (list (v! -1 -1 0)
						(v! 0 0)))
				    :dimensions 6 :element-type 'g-pt))
	    (,vertex-stream (make-buffer-stream ,array)))
       (let ((,result (progn ,@body)))
	 (free ,vertex-stream)
	 (free ,array)
	 ,result))))

(defmacro with-quarter ((vertex-stream) &body body)
  (let ((result (gensym "result"))
	 (array (gensym "array")))
    `(let* ((,array (make-gpu-array (list (list (v! -1 -1 0)
						(v! 0 0))
					  (list (v! -1 0 0)
						(v! 0 1))
					  (list (v! 0 0 0)
						(v! 1 1))
					  (list (v! 0 0 0)
						(v! 1 1))
					  (list (v! 0 -1 0)
						(v! 1 0))
					  (list (v! -1 -1 0)
						(v! 0 0)))
				    :dimensions 6 :element-type 'g-pt))
	    (,vertex-stream (make-buffer-stream ,array)))
       (let ((,result (progn ,@body)))
	 (free ,vertex-stream)
	 (free ,array)
	 ,result))))

(defmacro with-rect ((vertex-stream width height) &body body)
  (let ((vert-list (gensym "vert-list"))
	(array (gensym "array"))
	(result (gensym "result")))
    `(let* ((,vert-list (list (list (v! 0 0 0)
						(v! 0 0))
					  (list (v! ,width 0 0)
						(v! 1 0))
					  (list (v! ,width ,height 0)
						(v! 1 1))
					  (list (v! ,width ,height 0)
						(v! 1 1))
					  (list (v! 0 ,height 0)
						(v! 0 1))
					  (list (v! 0 0 0)
						(v! 0 0))))
	    (,array (make-gpu-array ,vert-list :dimensions 6 :element-type 'g-pt))
	    (,vertex-stream (make-buffer-stream ,array)))
       (let ((,result (progn ,@body)))
	 (free ,vertex-stream)
	 (free ,array)
	 ,result))))

(defun delete-effect (effect)
  (cepl:free (fbo effect)))

(defun get-or-make-fbo (surface effect)
  (with-slots (width height) surface
    (if (and (fbo effect) (= width (width effect)) (= height (height effect)))
	(fbo effect)
	(progn
	  (format t "Making new framebuffer (dimensions: ~Ax~A) ~%" width height)
	  (when (fbo effect)
	    (cepl:free (fbo effect)))
	  (setf (width effect) width)
	  (setf (height effect) height)
	  (let ((fbo (cepl:make-fbo `(0 :dimensions (,width ,height)))))
	    (setf (cepl:blending-params fbo) (cepl:make-blending-params :source-alpha :one))
	    (setf (fbo effect) fbo)
	    (setf (fbo-attachment effect) (cepl:attachment-tex fbo 0))
	    (setf (fbo-sample effect) (cepl:sample (fbo-attachment effect)))
	    fbo)))))

(defclass effect ()
  ((fbo :accessor fbo :initarg :fbo :initform nil)
   (fbo-attachment :accessor fbo-attachment :initarg :fbo-attachment :initform nil)
   (fbo-sample :accessor fbo-sample :initarg :fbo-sample :initform nil)
   (blending-parameters :accessor blending-parameters :initarg :blending-parameters :initform nil)
   (width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)
   (pipeline :accessor pipeline :initarg :pipeline :initform nil)))

#|
(defmethod add-effect ((surface surface) pipeline)
  (with-slots (width height effects) surface
    (let* ((fbo (cepl:make-fbo `(0 :dimensions (,width ,height))))
	   (fbo-attachment (cepl:attachment-tex fbo 0)))
      (setf (cepl:blending-params fbo) (cepl:make-blending-params :destination-alpha :one :source-alpha :one))
      (setf effects (cons (make-instance 'effect :width width :height height :pipeline pipeline :fbo fbo :fbo-attachment fbo-attachment :fbo-sample (cepl:sample fbo-attachment) :blending-parameters (cepl:make-blending-params)) effects)))))
|#


(defmethod add-effect ((surface surface) pipeline)
  (with-slots (width height effects) surface
    (push (make-instance 'effect :width width :height height :pipeline pipeline) effects)))

(defmacro map-g-default/fbo (fbo pipeline vertex-stream &rest uniforms)
  `(if ,fbo
       (cepl:map-g-into ,fbo ,pipeline ,vertex-stream ,@uniforms)
       (cepl:map-g ,pipeline ,vertex-stream ,@uniforms)))

;; Each screen to have its own framebuffer?
;; Let's 

(defmethod texture-of ((surface surface))
  "Given a surface will return a texture sampler of either the underlying texture or a FBO which has been used to apply effects to the surface"
  (with-slots (effects) surface
    (with-screen (ys)
      (let ((tex (cepl-texture (texture surface))))
	(if effects
	    (let (dest-fbo)
	      (loop :for effect :in (reverse effects)
		 :for i :from 0 :to (- (length effects) 1)
		 :do (progn
		       (setf dest-fbo (get-or-make-fbo surface effect))
		       (let ((sampler (if (> i 0)
					  (cepl:sample (fbo-attachment (nth (- i 1) effects)))
					  (cepl:sample tex))))
			 (gl:viewport 0 0 (width surface) (height surface))
			 (gl:disable :blend) ;; We just want to copy into a blank FBO
			 (clear dest-fbo) ;This makes shadows disappear
			 (cepl:map-g-into dest-fbo (pipeline effect) ys :texture sampler)
			 (gl:enable :blend)))
		 :finally (return-from texture-of (fbo-sample effect))))
	    (cepl:sample tex))))))

(defmethod texture-of :after ((surface surface)); map-pipleine &optional final-fbo)
  (when (->frame-callback surface)
    (wl-callback-send-done (->frame-callback surface) (get-milliseconds))
    (wl-resource-destroy (->frame-callback surface))
    (setf (->frame-callback surface) nil)))

(defmacro with-surface ((vertex-stream tex mode surface &key (fbo nil) (z 0) (scale 1.0)) &body body)
  (let ((x (gensym "x"))
	(y (gensym "y"))
	(texture (gensym "texture"))
	(width (gensym "width"))
	(height (gensym "height"))
	(array (gensym "array")))
    `(let* ((,x (x ,surface))
	    (,y (y ,surface))
	    (,texture (texture ,surface))
	    (,width (width ,texture))
	    (,height (height ,texture))
	    (,array (make-gpu-array (list (list (v! 0 0 ,z)
						(v! 0 0))
					  (list (v! ,width 0 ,z)
						(v! 1 0))
					  (list (v! ,width ,height ,z)
						(v! 1 1))
					  (list (v! ,width ,height ,z)
						(v! 1 1))
					  (list (v! 0 ,height ,z)
						(v! 0 1))
					  (list (v! 0 0 ,z)
						(v! 0 0)))
				    :dimensions 6 :element-type 'g-pt))
	    (,vertex-stream (make-buffer-stream ,array))
	    (,tex (cepl-texture ,texture)))
       (with-blending (blending-parameters ,mode)
	 ,@body)
       (free ,vertex-stream)
       (free ,array)

       (when (->frame-callback ,surface)
	 (wl-callback-send-done (->frame-callback ,surface) (get-milliseconds))
	 (wl-resource-destroy (->frame-callback ,surface))
	 (setf (->frame-callback ,surface) nil)))))

(defun ortho (left right bottom top near far)
  (let ((m (m4:identity)))
    (setf (m4:melm m 0 0) (/ 2.0 (- right left)))
    (setf (m4:melm m 1 1) (/ 2.0 (- top bottom)))
    (setf (m4:melm m 2 2) (/ -2.0 (- far near)))
    (setf (m4:melm m 0 3) (coerce (- (/ (+ right left) (- right left))) 'float))
    (setf (m4:melm m 1 3) (coerce (- (/ (+ top bottom) (- top bottom))) 'float))
    (setf (m4:melm m 2 3) (coerce (- (/ (+ far near) (- far near))) 'float))
    m))

(defun-g passthrough-vert ((vert g-pt))
  (values (v! (pos vert) 1) (tex vert)))

(defun-g passthrough-frag ((tex-coord :vec2) &uniform (texture :sampler-2d))
  (texture texture tex-coord))

(def-g-> passthrough-shader ()
  #'passthrough-vert #'passthrough-frag)

(defun-g default-vertex-shader ((vert g-pt) &uniform (ortho :mat4) (surface-scale :mat4) (surface-translate :mat4))
  (values (* ortho surface-translate surface-scale (v! (pos vert) 1))
	  (:smooth (tex vert))))

(defun-g default-fragment-shader ((tex-coord :vec2) &uniform (texture :sampler-2d) (alpha :float))
  (v! (s~ (texture texture tex-coord) :xyz)
      (* alpha (s~ (texture texture tex-coord) :w))))

(defun-g cursor-vertex-shader ((vert :vec3) &uniform (ortho :mat4))
  (values (* ortho (v! vert 1))
	  (v! 1 1 1 1)))

(defun-g cursor-fragment-shader ((color :vec4))
  color)

(def-g-> cursor-pipeline ()
  #'cursor-vertex-shader #'cursor-fragment-shader)

(defun draw-cursor (x y ortho)
  (let* ((array (make-gpu-array
		 (list (v! x y 0)
		       (v! x (+ y 32) 0)
		       (v! (+ x 16) (+ y 24) 0))
		 :dimensions 3 :element-type :vec3))
	 (vertex-stream (make-buffer-stream array)))
    (map-g #'cursor-pipeline vertex-stream
	   :ortho ortho)
    (free vertex-stream)
    (free array)
    (setf (render-needed *compositor*) t)))
