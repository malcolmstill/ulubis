
(in-package :ulubis)

(defclass texture-gl ()
  ((width :accessor width :initarg :width :initform 0)
   (height :accessor height :initarg :height :initform 0)
   (cepl-texture :accessor cepl-texture :initarg :cepl-texture :initform nil)))

(defun create-texture (surface)
  (when (and (buffer surface) (not (pointer-eq (buffer surface) (null-pointer))))
    (let ((egl-display (cepl.drm-gbm::get-egl-display)))
      (with-foreign-object (texture-format :uint32)
	(if (not (= 0 (egl:query-wayland-buffer egl-display (buffer surface) #x3080 texture-format)))
	    (create-texture-egl egl-display surface (mem-aref texture-format :uint32))
	    (create-texture-shm surface))))))

(defun create-texture-egl (display surface texture-format)
  (with-foreign-objects ((width :uint32) (height :uint32) (attribs :uint32))
    (egl:query-wayland-buffer display (buffer surface) #x3057 width)
    (egl:query-wayland-buffer display (buffer surface) #x3056 height)
    (let ((width (mem-aref width :uint32))
	  (height (mem-aref width :uint32)))
      (setf (width surface) width)
      (setf (height surface) height)
      (setf (mem-ref attribs :int32) #x3038) ;; EGL_NONE
      (let ((image (egl:create-image display (cepl.drm-gbm::get-egl-context) #x31D5 (buffer surface) attribs)))
	(when (and (texture surface) (cepl-texture (texture surface)))
	  (cepl:free (cepl-texture (texture surface))))
	(setf (texture surface) (make-egl-texture image width height))
	(wl-buffer-send-release (buffer surface))
	(setf (buffer surface) (null-pointer))))))


(defun create-texture-shm (surface)
  (let* ((buffer (buffer surface))
	 (shm-buffer (wl-shm-buffer-get buffer))
	 (width (wl-shm-buffer-get-width shm-buffer))
	 (height (wl-shm-buffer-get-height shm-buffer))
	 (stride (wl-shm-buffer-get-stride shm-buffer))
	 (image (wl-shm-buffer-get-data shm-buffer)))
	 ;; (array (cepl:make-c-array-from-pointer
	 ;; 	 (list w h)
	 ;; 	 :uint8-vec4 
	 ;; 	 (wl-shm-buffer-get-data shm-buffer))))
    (setf (width surface) width)
    (setf (height surface) height)
    (when (and (texture surface) (cepl-texture (texture surface)))
      (cepl:free (cepl-texture (texture surface))))
    (setf (texture surface) (make-gl-texture image width height))
	  ;; (make-instance 'texture-gl
	  ;; 				   :width w
	  ;; 				   :height h
	  ;; 				   :cepl-texture
	  ;; 				   (cepl:make-texture
	  ;; 				    array
	  ;; 				    :element-type :rgba8)))
    ;; Copy pixels from shared-memory buffer to SDL texture
    (wl-buffer-send-release buffer)
    (setf (buffer surface) (null-pointer))))

(defun make-egl-texture (image width height)
  (let* ((ids (gl:gen-textures 1))
	 (id (first ids)))
    (gl:bind-texture :texture-2d id)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (egl:image-target-texture-2DOES #xDE1 image)
    (gl:bind-texture :texture-2d 0)
    (make-instance 'texture-gl
		   :width width
		   :height height
		   :cepl-texture (cepl:make-texture-from-id id
							    :mutable-p nil
							    :fixed-sample-locations t
							    :allocated t
							    :texture-type :texture-2d
							    :element-type :rgba8
							    :base-dimensions (list width height)
							    ))))

(defun make-gl-texture (image width height)
  (let* ((ids (gl:gen-textures 1))
	 (id (first ids)))
    (gl:bind-texture :texture-2d id)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-image-2d :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte image)
    (gl:bind-texture :texture-2d 0)
    (make-instance 'texture-gl
		   :width width
		   :height height
		   :cepl-texture (cepl:make-texture-from-id id :element-type :rgba8 :base-dimensions '(? ?) ))))


(defmacro with-blending-on-fbo (blending-params fbo &body body)
  (let ((b-params (gensym "blending-params")))
    `(let* ((,b-params ,blending-params))
       (cepl.blending::%with-blending ,fbo nil ,b-params
         ,@body))))

(defmacro with-screen ((vertex-stream) &body body)
  (let ((result (gensym "result"))
	 (array (gensym "array")))
    `(let* ((,array (cepl:make-gpu-array (list (list (rtg-math:v! -1 -1 0)
						     (rtg-math:v! 0 0))
					       (list (rtg-math:v! -1 1 0)
						     (rtg-math:v! 0 1))
					       (list (rtg-math:v! 1 1 0)
						(rtg-math:v! 1 1))
					  (list (rtg-math:v! 1 1 0)
						(rtg-math:v! 1 1))
					  (list (rtg-math:v! 1 -1 0)
						(rtg-math:v! 1 0))
					  (list (rtg-math:v! -1 -1 0)
						(rtg-math:v! 0 0)))
				    :dimensions 6 :element-type 'cepl:g-pt))
	    (,vertex-stream (cepl:make-buffer-stream ,array)))
       (let ((,result (progn ,@body)))
	 (cepl:free ,vertex-stream)
	 (cepl:free ,array)
	 ,result))))

(defmacro with-quarter ((vertex-stream) &body body)
  (let ((result (gensym "result"))
	 (array (gensym "array")))
    `(let* ((,array (cepl:make-gpu-array (list (list (rtg-math:v! -1 -1 0)
						     (rtg-math:v! 0 0))
					       (list (rtg-math:v! -1 0 0)
						     (rtg-math:v! 0 1))
					       (list (rtg-math:v! 0 0 0)
						     (rtg-math:v! 1 1))
					       (list (rtg-math:v! 0 0 0)
						     (rtg-math:v! 1 1))
					       (list (rtg-math:v! 0 -1 0)
						     (rtg-math:v! 1 0))
					       (list (rtg-math:v! -1 -1 0)
						     (rtg-math:v! 0 0)))
					 :dimensions 6 :element-type 'cepl:g-pt))
	    (,vertex-stream (cepl:make-buffer-stream ,array)))
       (let ((,result (progn ,@body)))
	 (cepl:free ,vertex-stream)
	 (cepl:free ,array)
	 ,result))))

(defmacro with-rect ((vertex-stream width height) &body body)
  (let ((vert-list (gensym "vert-list"))
	(array (gensym "array"))
	(result (gensym "result")))
    `(let* ((,vert-list (list (list (rtg-math:v! 0 0 0)
						(rtg-math:v! 0 0))
					  (list (rtg-math:v! ,width 0 0)
						(rtg-math:v! 1 0))
					  (list (rtg-math:v! ,width ,height 0)
						(rtg-math:v! 1 1))
					  (list (rtg-math:v! ,width ,height 0)
						(rtg-math:v! 1 1))
					  (list (rtg-math:v! 0 ,height 0)
						(rtg-math:v! 0 1))
					  (list (rtg-math:v! 0 0 0)
						(rtg-math:v! 0 0))))
	    (,array (cepl:make-gpu-array ,vert-list :dimensions 6 :element-type 'cepl:g-pt))
	    (,vertex-stream (cepl:make-buffer-stream ,array)))
       (let ((,result (progn ,@body)))
	 (cepl:free ,vertex-stream)
	 (cepl:free ,array)
	 ,result))))

(defun delete-effect (effect)
  (cepl:free (fbo effect)))

(defun get-or-make-fbo (surface effect)
  (with-slots (width height) surface
    (if (and (fbo effect) (= width (width effect)) (= height (height effect)))
	(fbo effect)
	(progn
	  ;;(format t "Making new framebuffer (dimensions: ~Ax~A) ~%" width height)
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


#|
(defmethod add-effect ((surface ulubis-surface) pipeline)
  (with-slots (width height effects) surface
    (push (make-instance 'effect :width width :height height :pipeline pipeline) effects)))
|#

(defmacro map-g-default/fbo (fbo pipeline vertex-stream &rest uniforms)
  `(if ,fbo
       (cepl:map-g-into ,fbo ,pipeline ,vertex-stream ,@uniforms)
       (cepl:map-g ,pipeline ,vertex-stream ,@uniforms)))

;; Each screen to have its own framebuffer?
;; Let's 

(defgeneric texture-of (surface)
  (:documentation "Given a surface will return a texture sampler of either the underlying texture or a FBO which has been used to apply effects to the surface"))

(defmethod texture-of ((surface isurface))
  ;;(describe surface)
  (with-slots (effects wl-surface) surface
    ;;(format t "Effects ~A~%" effects)
    (with-slots (width height texture) wl-surface
      (with-screen (ys)
	(let ((tex (cepl-texture texture)))
	  (if effects
	      (let (dest-fbo)
		(loop :for effect :in (reverse effects)
		   :for i :from 0 :to (- (length effects) 1)
		   :do (progn
			 (setf dest-fbo (get-or-make-fbo surface effect))
			 (let ((sampler (if (> i 0)
					    (cepl:sample (fbo-attachment (nth (- i 1) effects)))
					    (cepl:sample tex))))
			   (gl:viewport 0 0 width height)
			   (gl:disable :blend) ;; We just want to copy into a blank FBO
			   (clear dest-fbo) ;This makes shadows disappear
			   (cepl:map-g-into dest-fbo (pipeline effect) ys :texture sampler)
			   (gl:enable :blend)))
		   :finally (return-from texture-of (fbo-sample effect))))
	      (cepl:sample tex)))))))

#|
(defmethod texture-of :after ((surface isurface)); map-pipleine &optional final-fbo)
  ;;(format t "FRAME CALLBACK~%")
  (with-slots (wl-surface) surface
    (with-slots (frame-callback) wl-surface
      (when frame-callback
	(wl-callback-send-done (->resource frame-callback) (get-milliseconds))
	(wl-resource-destroy (->resource frame-callback))
	(remove-resource frame-callback)
	(setf frame-callback nil)))))
|#

(defmacro with-surface ((vertex-stream tex mode surface &key (fbo nil) (z 0) (scale 1.0)) &body body)
  (let ((x (gensym "x"))
	(y (gensym "y"))
	(texture (gensym "texture"))
	(width (gensym "width"))
	(height (gensym "height"))
	(array (gensym "array")))
    `(let* ((,x (x ,surface))
	    (,y (y ,surface))
	    (,texture (texture (wl-surface ,surface)))
	    (,width (width (wl-surface ,texture)))
	    (,height (height (wl-surface ,texture)))
	    (,array (cepl:make-gpu-array (list (list (rtg-math:v! 0 0 ,z)
						     (rtg-math:v! 0 0))
					       (list (rtg-math:v! ,width 0 ,z)
						     (rtg-math:v! 1 0))
					       (list (rtg-math:v! ,width ,height ,z)
						     (rtg-math:v! 1 1))
					       (list (rtg-math:v! ,width ,height ,z)
						     (rtg-math:v! 1 1))
					       (list (rtg-math:v! 0 ,height ,z)
						     (rtg-math:v! 0 1))
					       (list (rtg-math:v! 0 0 ,z)
						     (rtg-math:v! 0 0)))
					 :dimensions 6 :element-type 'cepl:g-pt))
	    (,vertex-stream (cepl:make-buffer-stream ,array))
	    (,tex (cepl-texture ,texture)))
       (cepl:with-blending (blending-parameters ,mode)
	 ,@body)
       (cepl:free ,vertex-stream)
       (cepl:free ,array)

       (when (frame-callback ,surface)
	 (wl-callback-send-done (->resource (frame-callback ,surface)) (get-milliseconds))
	 (wl-resource-destroy (->resource (frame-callback ,surface)))
	 (setf (frame-callback ,surface) nil)))))

(defun ortho (left right bottom top near far)
  (let ((m (m4:identity)))
    (setf (m4:melm m 0 0) (/ 2.0 (- right left)))
    (setf (m4:melm m 1 1) (/ 2.0 (- top bottom)))
    (setf (m4:melm m 2 2) (/ -2.0 (- far near)))
    (setf (m4:melm m 0 3) (coerce (- (/ (+ right left) (- right left))) 'float))
    (setf (m4:melm m 1 3) (coerce (- (/ (+ top bottom) (- top bottom))) 'float))
    (setf (m4:melm m 2 3) (coerce (- (/ (+ far near) (- far near))) 'float))
    m))

(cepl:defun-g passthrough-vert ((vert cepl:g-pt))
  (values (rtg-math:v! (cepl:pos vert) 1) (cepl:tex vert)))

(cepl:defun-g passthrough-frag ((tex-coord :vec2) &uniform (texture :sampler-2d))
  (cepl:texture texture tex-coord))

(cepl:defpipeline-g passthrough-shader ()
  (passthrough-vert cepl:g-pt) (passthrough-frag :vec2))

(cepl:defun-g default-vertex-shader ((vert cepl:g-pt) &uniform (ortho :mat4) (surface-scale :mat4) (surface-translate :mat4))
  (values (* ortho surface-translate surface-scale (rtg-math:v! (cepl:pos vert) 1))
	  (:smooth (cepl:tex vert))))

#|
Wayland surfaces come in in BGR (I believe) so we swap to RGB here
|#
(cepl:defun-g default-fragment-shader ((tex-coord :vec2) &uniform (texture :sampler-2d) (alpha :float))
  (rtg-math:v! (rtg-math:s~ (cepl:texture texture tex-coord) :z)
	   (rtg-math:s~ (cepl:texture texture tex-coord) :y)
	   (rtg-math:s~ (cepl:texture texture tex-coord) :x)
	   (* alpha (rtg-math:s~ (cepl:texture texture tex-coord) :w))))

(cepl:defun-g default-rgb-frag ((tex-coord :vec2) &uniform (texture :sampler-2d) (alpha :float))
  (rtg-math:v! (rtg-math:s~ (cepl:texture texture tex-coord) :x)
	       (rtg-math:s~ (cepl:texture texture tex-coord) :y)
	       (rtg-math:s~ (cepl:texture texture tex-coord) :z)
	       (* alpha (rtg-math:s~ (cepl:texture texture tex-coord) :w))))

(cepl:defun-g ulubis-cursor-vertex-shader ((vert cepl:g-pt) &uniform (ortho :mat4) (origin :mat4) (origin-inverse :mat4) (surface-scale :mat4) (surface-translate :mat4))
  (values (* ortho surface-translate origin-inverse surface-scale origin (rtg-math:v! (cepl:pos vert) 1))
	  (:smooth (cepl:tex vert))))

(cepl:defpipeline-g ulubis-cursor-pipeline ()
  (ulubis-cursor-vertex-shader cepl:g-pt) (default-fragment-shader :vec2))
    
(defmethod draw-cursor ((surface isurface) fbo x y ortho)
  ;;(format t "DRAW-CURSOR~%")
  ;;(describe surface)
  (when (texture (wl-surface surface))
    (with-rect (vertex-stream (width (wl-surface surface)) (height (wl-surface surface)))
      (let ((texture (texture-of surface)))
	(gl:viewport 0 0 (screen-width *compositor*) (screen-height *compositor*))
	(map-g-default/fbo fbo #'ulubis-cursor-pipeline vertex-stream
			   :ortho ortho
			   :origin (m4:translation (rtg-math:v! (- (origin-x surface)) (- (origin-y surface)) 0))
			   :origin-inverse (m4:translation (rtg-math:v! (origin-x surface) (origin-y surface) 0))
			   :surface-scale (m4:scale (rtg-math:v! (scale-x surface) (scale-y surface) 1.0))
			   :surface-translate (m4:translation (rtg-math:v! (- x (x surface)) (- y (y surface)) 0.0))
			   :texture texture
			   :alpha (opacity surface))))))

(cepl:defun-g cursor-vertex-shader ((vert cepl:g-pt) &uniform (ortho :mat4))
  (values (* ortho (rtg-math:v! (cepl:pos vert) 1))
	  (cepl:tex vert)))

(cepl:defpipeline-g cursor-pipeline ()
  (cursor-vertex-shader cepl:g-pt) (passthrough-frag :vec2))

(defparameter *default-cursor* nil)

(defun init-vector-cursor ()
  (unless *default-cursor*
    (setf *default-cursor* (make-instance 'cairo-surface
                                          :allow-gl t
                                          :width 64
                                          :height 64))
    (setf (draw-func *default-cursor*)
          (lambda ()
            (cl-cairo2:translate 32 32)
            (cl-cairo2:set-source-rgba 1 1 1 0)
            (cl-cairo2:paint)
            (cl-cairo2:move-to 0 0)
            (cl-cairo2:line-to 0 15)
            (cl-cairo2:line-to 4 13)
            (cl-cairo2:line-to 6 20)
            (cl-cairo2:line-to 8 19)
            (cl-cairo2:line-to 6 12)
            (cl-cairo2:line-to 9 12)
            (cl-cairo2:close-path)
            (cl-cairo2:set-source-rgba 0 0 0 1)
            (cl-cairo2:stroke-preserve)
            (cl-cairo2:set-source-rgba 1 1 1 1)
            (cl-cairo2:fill-path)
            ))))

(defun init-image-cursor ()
  (unless *default-cursor*
    (setf *default-cursor* (make-instance 'cairo-surface
                                          :allow-gl t
                                          :filename "assets/cursor.png"))))

(defun make-g-pt-quad (top bottom left right)
  `((,(rtg-math:v! left top 0)     ,(rtg-math:v! 0 0))
    (,(rtg-math:v! left bottom 0)  ,(rtg-math:v! 0 1))
    (,(rtg-math:v! right top 0)    ,(rtg-math:v! 1 0))
    (,(rtg-math:v! right bottom 0) ,(rtg-math:v! 1 1))
    (,(rtg-math:v! right top 0)    ,(rtg-math:v! 1 0))
    (,(rtg-math:v! left bottom 0)  ,(rtg-math:v! 0 1))))

(defmethod draw-cursor ((cursor (eql nil)) fbo x y ortho)
  (unless *default-cursor*
    (init-image-cursor)
    (cairo-surface-redraw *default-cursor*))
  (let* ((halfw (/ (width *default-cursor*) 2))
         (halfh (/ (height *default-cursor*) 2))
         (array (cepl:make-gpu-array (make-g-pt-quad (- y halfh)
                                                     (+ y halfh)
                                                     (- x halfw)
                                                     (+ x halfw))
                                     :element-type 'cepl:g-pt))
	 (vertex-stream (cepl:make-buffer-stream array)))
    (map-g-default/fbo fbo #'cursor-pipeline vertex-stream
                       :ortho ortho
                       :texture (texture-of *default-cursor*))
    (cepl:free vertex-stream)
    (cepl:free array)
    (setf (render-needed *compositor*) t)))

#|
(defgeneric render-surface (surface mode))
|#
