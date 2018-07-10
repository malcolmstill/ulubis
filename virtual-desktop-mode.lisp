
(in-package :ulubis)

(defparameter *ortho* (m4:identity))

#|

Our virtual desktop mode. The virtual desktop mode will control
a number of views (defined by the user). This control includes
keybindings for switching between the views and the rendering of
views.

Initially we'll have two key combinations for moving left or right
to the prev / next virtual desktop. The key combinations will
activate a slide animation to smoothly transition between the
views.

When the slide-animation is in effect (when it is non null) we'll
render either all the views or only those visible for the transition.

When the slide-animation is not in effect (when it is null) we'll
only render the visible view.

|#

(defmode virtual-desktop-mode ()
  ((clear-color :accessor clear-color
		:initarg :clear-color
		:initform (list 0.3 0.3 0.3 0.0))
   (projection :accessor projection
	       :initarg :projection
	       :initform (m4:identity))
   (old-surface :accessor old-surface
		:initarg :old-surface
		:initform nil)
   (new-surface :accessor new-surface
		:initarg :new-surface
		:initform nil)
   (slide-animation :accessor slide-animation
		    :initarg :slide-animation
		    :initform nil)
   (focus-follows-mouse :accessor focus-follows-mouse
			:initarg :focus-follows-mouse
			:initform nil)))

(defmethod init-mode ((mode virtual-desktop-mode))
  (setf *ortho* (ortho 0 (screen-width *compositor*) (screen-height *compositor*) 0 1 -1))
  (cepl:map-g #'mapping-pipeline nil)
  (setf (render-needed *compositor*) t))

(defkeybinding (:pressed "q" Ctrl Shift) () (virtual-desktop-mode)
  (uiop:quit))

(defkeybinding (:pressed "s" Ctrl Shift) () (virtual-desktop-mode)
  (screenshot))

#|
Here we set up a keybinding fro swicthing between views (virtual desktops).
Our mode knows the view that it is on. It finds all views on the compositor
and the position of its view. It therefore knows which view to set as the
current-view.

However, we have to define this in the mode. If we have a bunch of different
modes across our views, we need to implement this logic on each mode. This
seems silly.

We should have a mode stack that sits above the views that defines this behaviour
and that would allow for animations of these modes. E.g. a pan from one view to
the next.
|#
(defkeybinding (:pressed "Right" Gui) (mode)
  (virtual-desktop-mode)
  (unless (slide-animation mode)
    (with-slots ((screen view)) mode
      (let* ((views (surfaces screen))
	     (current-view (active-surface screen))
	     (count (length views))
	     (pos (position current-view views)))
	(when (not (= pos (- count 1)))
	  (make-slide-animation mode screen current-view (nth (+ pos 1) views) 1))))
    (setf (render-needed *compositor*) t)))

(defkeybinding (:pressed "Left" Gui) (mode)
  (virtual-desktop-mode)
  (unless (slide-animation mode)
    (with-slots ((screen view)) mode
      (let* ((views (surfaces screen))
	     (current-view (active-surface screen))
	     (pos (position current-view views)))
	(when (not (= pos 0))
	  (make-slide-animation mode screen current-view (nth (- pos 1) views) -1))))
    (setf (render-needed *compositor*) t)))

(defun make-slide-animation (mode screen old-surface new-surface mult)
  (setf (x new-surface) (* mult (screen-width *compositor*)))
  (setf (slide-animation mode)
	(parallel-animation (lambda ()
			      (setf (active-surface screen) new-surface)
			      (setf (x new-surface) 0)
			      (setf (slide-animation mode) nil)
			      (setf (old-surface mode) nil)
			      (setf (new-surface mode) nil))
			    (animation :duration 150
				       :target new-surface
				       :property 'x
				       :to 0
				       :easing-fn 'easing:in-out-exp)
			    (animation :duration 150
				       :target old-surface
				       :property 'x
				       :to (* -1 mult (screen-width *compositor*))
				       :easing-fn 'easing:in-out-exp)))
  (setf (old-surface mode) old-surface)
  (setf (new-surface mode) new-surface)
  (start-animation (slide-animation mode)))

(cepl:defun-g vd-vert ((vert cepl:g-pt)
		       &uniform
		       (origin :mat4)
		       (origin-inverse :mat4)
		       (surface-scale :mat4)
		       (surface-translate :mat4))
  (values (* *ortho*
	     surface-translate
	     origin-inverse
	     surface-scale
	     origin
	     (rtg-math:v! (cepl:pos vert) 1))
	  (:smooth (cepl:tex vert))))

(cepl:defpipeline-g vd-pipeline ()
  (vd-vert cepl:g-pt)
  (default-rgb-frag :vec2))

(defmethod render ((surface view) &optional desktop-fbo)
  (with-rect (vertex-stream (screen-width *compositor*) (screen-height *compositor*))
    (let ((texture (texture-of surface)))
      (map-g-default/fbo desktop-fbo #'vd-pipeline vertex-stream
			 :origin (m4:translation (rtg-math:v! (- (/ (screen-width *compositor*) 2))
							      (- (/ (screen-height *compositor*) 2))
							      0))
			 :origin-inverse (m4:translation (rtg-math:v! (/ (screen-width *compositor*) 2)
								      (/ (screen-height *compositor*) 2)
								      0))
			 :surface-scale (m4:scale (rtg-math:v! (scale-x surface)
							       (* -1.0 (scale-y surface))
							       1.0))
			 :surface-translate (m4:translation (rtg-math:v! (x surface)
									 (y surface)
									 0.0))
			 :texture texture
			 :alpha 1.0))))

(defmethod render ((mode virtual-desktop-mode) &optional desktop-fbo)
  (apply #'gl:clear-color (clear-color mode))
  (when desktop-fbo
    (cepl:clear desktop-fbo))
  (if (not (slide-animation mode))
    ;; static view of single virtual desktop
    (cepl:with-blending (blending-parameters mode)
      (let ((view (active-surface (view mode))))
	(setf (x view) 0)
	(render view desktop-fbo)))
    ;; If we are transitioning draw the two virtual desktops involved
    (mapcar (lambda (virtual-destkop)
	      (cepl:with-blending (blending-parameters mode)
		(render virtual-destkop desktop-fbo)))
	    (list (old-surface mode) (new-surface mode)))))
