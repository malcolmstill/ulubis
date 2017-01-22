
(in-package :ulubis)

(defmode alt-tab-mode ()
  ((clear-color :accessor clear-color :initarg :clear-color :initform (list 0.3 0.3 0.3 0.0))
   (projection :accessor projection :initarg :projection :initform (m4:identity))
   (selection :accessor selection :initarg :selection :initform 0)
   (surfaces :accessor surfaces :initarg :surfaces :initform nil)
   (y-angle :accessor y-angle :initarg :y-angle :initform 0.0)
   (x-angle :accessor x-angle :initarg :x-angle :initform 0.0)
   (iso-animation :accessor iso-animation :initarg :iso-animation :initform nil)
   (opacity :accessor opacity :initarg :opacity :initform 1.0)))

(defun enter-animation (mode)
  (parallel-animation nil
   (animation :duration 150 :target mode :property 'opacity :to 0.15 :easing-fn 'easing:linear)))

(defun exit-animation (mode)
  (parallel-animation (lambda () (pop-mode mode))
   (animation :duration 150 :target mode :property 'opacity :to 1.0 :easing-fn 'easing:linear)))

(defmethod init-mode ((mode alt-tab-mode))
  (setf (surfaces mode) (remove-if (lambda (surface)
				     (not (texture (wl-surface surface))))
				   (surfaces (view mode))))
  (setf (projection mode) (ortho 0 (screen-width *compositor*) (screen-height *compositor*) 0 1000 -1000))
  (setf (iso-animation mode) (enter-animation mode))
  (start-animation (iso-animation mode)))

(defmethod mouse-motion-handler ((mode alt-tab-mode) time delta-x delta-y)
  )

(defmethod mouse-button-handler ((mode alt-tab-mode) time button state)
  )

(defkeybinding (:pressed 15) (mode) (alt-tab-mode)
  (with-slots (surfaces selection) mode
    (when (> (length surfaces) 0)
      (setf selection (mod (incf selection) (length surfaces)))
      (setf (render-needed *compositor*) t))))

(defkeybinding (:released nil Gui) (mode) (alt-tab-mode)
  (format t "Released alt-tab ~%")
  (with-slots (surfaces selection iso-animation) mode
    (setf (render-needed *compositor*) t)
    (when iso-animation
      (stop-animation iso-animation))
    (let ((selected-surface (nth selection surfaces)))
      (raise-surface selected-surface (view mode))
      (activate-surface selected-surface mode))
    (start-animation (exit-animation mode))))

(defun rot-y (angle)
  (m4:rotation-from-axis-angle (cepl:v! 0 1 0) angle))

(defun rot-x (angle)
  (m4:rotation-from-axis-angle (cepl:v! 1 0 0) angle))

(cepl:defun-g alt-tab-vertex-shader ((vert cepl:g-pt) &uniform (surface-scale :mat4) (surface-translate :mat4) (ortho :mat4) (rot-y :mat4) (rot-x :mat4))
  (values (* rot-x (* rot-y (* ortho (* surface-translate (* surface-scale (cepl:v! (cepl:pos vert) 1))))))
	  (:smooth (cepl:tex vert))))

(cepl:defun-g alt-tab-frag ((tex-coord :vec2) &uniform (texture :sampler-2d) (alpha :float))
  (cepl:v!
   (cepl:s~ (cepl:texture texture tex-coord) :z)
   (cepl:s~ (cepl:texture texture tex-coord) :y)
   (cepl:s~ (cepl:texture texture tex-coord) :x)
   (* alpha (cepl:s~ (cepl:texture texture tex-coord) :w))))

(cepl:def-g-> alt-tab-pipeline ()
  (alt-tab-vertex-shader cepl:g-pt) (alt-tab-frag :vec2))

(defmethod render ((mode alt-tab-mode) &optional view-fbo)
  (let* ((drawable-surfaces (surfaces mode))
	 (surface-count (length drawable-surfaces))
	 (order (reverse (loop :for i :from 0 :to (- surface-count 1) :collecting i)))
	 (spacing (if (> surface-count 0)
		      (/ (screen-height *compositor*) surface-count)
		      0)))
    (apply #'gl:clear-color (clear-color mode))
    (cepl:clear)

    (mapcar (lambda (surface o)
	      (cepl:with-blending (blending-parameters mode)
		(with-rect (vs (width (wl-surface surface)) (height (wl-surface surface)))
		  ;;	      (with-surface (vs tex mode surface :z (+ (* (- o) spacing) 100))
		  (let ((tex (texture-of surface)))
		    (map-g-default/fbo view-fbo #'alt-tab-pipeline vs
				       :surface-scale (m4:scale (cepl:v! (scale-x surface) (scale-y surface) 1.0))
				       :surface-translate (m4:translation (cepl:v! (x surface) (y surface) (+ (* (- o) spacing) 100)))
				       :ortho (projection mode)
				       :rot-y (rot-y (y-angle mode))
				       :rot-x (rot-x (x-angle mode))
				       :texture tex
				       :alpha (if (= (selection mode) o) 1.0 (opacity mode)))))))
	    (reverse drawable-surfaces)
	    order)))
