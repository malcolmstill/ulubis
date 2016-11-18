
(in-package :ulubis)

(defparameter *default-mode* nil)

(defmode desktop-mode ()
  ((clear-color :accessor clear-color :initarg :clear-color :initform (list 0.3 0.3 0.3 0.0))
   (projection :accessor projection :initarg :projection :initform (m4:identity))
   (focus-follows-mouse :accessor focus-follows-mouse :initarg :focus-follows-mouse :initform nil)))

(defmethod init-mode ((mode desktop-mode))
  (setf (projection mode) (ortho 0 (screen-width *compositor*) (screen-height *compositor*) 0 1 -1))
  (map-g #'mapping-pipeline nil)
  (setf (render-needed *compositor*) t))

(defun move-surface (x y move-op)
  "Move surface to location X and Y given the MOVE-OP"
  (let ((surface (move-op-surface move-op)))
    (setf (x surface) (round (+ (move-op-surface-x move-op) (- x (move-op-pointer-x move-op)))))
    (setf (y surface) (round (+ (move-op-surface-y move-op) (- y (move-op-pointer-y move-op)))))
    (setf (render-needed *compositor*) t)))

(defun resize-surface (x y view resize-op)
  "Resize surface given new pointer location (X,Y) and saved information in RESIZE-OP"
  (let* ((saved-width (resize-op-surface-width resize-op))
	 (saved-height (resize-op-surface-height resize-op))
	 (saved-pointer-x (resize-op-pointer-x resize-op))
	 (saved-pointer-y (resize-op-pointer-y resize-op))
	 (delta-x (- x saved-pointer-x))
	 (delta-y (- y saved-pointer-y)))
    (resize-surface-absolute (resize-op-surface resize-op) view (+ saved-width delta-x) (+ saved-height delta-y))))

(defun pointer-changed-surface (mode x y old-surface new-surface)
  (setf (cursor-surface *compositor*) nil)
  (when (focus-follows-mouse mode)
    (deactivate-surface old-surface)) 
  (when (accepts-pointer-events? old-surface)
    (wl-pointer-send-leave (->pointer (client old-surface))
			   0
			   (->surface old-surface)))
  (setf (pointer-surface *compositor*) new-surface)
  (when (focus-follows-mouse mode)
    (activate-surface new-surface (view mode)))
  (when (accepts-pointer-events? new-surface)
    (wl-pointer-send-enter (->pointer (client new-surface))
			   0
			   (->surface new-surface)
			   (round (* 256 (- x (x new-surface))))
			   (round (* 256 (- y (y new-surface)))))))

(defun send-surface-pointer-motion (x y time surface)
  (when (accepts-pointer-events? surface)
    (wl-pointer-send-motion (->pointer (client surface))
			    time
			    (round (* 256 (- x (x surface))))
			    (round (* 256 (- y (y surface)))))
    ;; Need to check client handles version 5
    ;;(wl-pointer-send-frame (->pointer (client surface)))))
    ))

(defun update-pointer (delta-x delta-y)
  (with-slots (pointer-x pointer-y screen-width screen-height) *compositor*
    (incf pointer-x delta-x)
    (incf pointer-y delta-y)
    (when (< pointer-x 0) (setf pointer-x 0))
    (when (< pointer-y 0) (setf pointer-y 0))
    (when (> pointer-x screen-width) (setf pointer-x screen-width))
    (when (> pointer-y screen-height) (setf pointer-y screen-height))))

(defmethod mouse-motion-handler ((mode desktop-mode) time delta-x delta-y)
  ;; Update the pointer location
  (with-slots (pointer-x pointer-y) *compositor*
    (update-pointer delta-x delta-y)
    (when (cursor-surface *compositor*)
      (setf (render-needed *compositor*) t))
    (let ((old-surface (pointer-surface *compositor*))
	  (current-surface (surface-under-pointer pointer-x pointer-y (view mode))))
      (cond
	;; 1. If we are dragging a window...
	((moving-surface *compositor*)
	 (move-surface pointer-x pointer-y (moving-surface *compositor*)))
	;; 2. If we are resizing a window...
	((resizing-surface *compositor*)
	 (resize-surface pointer-x pointer-y (view mode) (resizing-surface *compositor*)))
	;; 3. The pointer has left the current surface
	((not (equalp old-surface current-surface))
	 (pointer-changed-surface mode pointer-x pointer-y old-surface current-surface))
	;; 4. Pointer is over previous surface
	((equalp old-surface current-surface)
	 (send-surface-pointer-motion pointer-x pointer-y time current-surface))))))

(defun pulse-animation (surface)
  (setf (origin-x surface) (/ (width surface) 2))
  (setf (origin-y surface) (/ (height surface) 2))
  (sequential-animation nil
			(parallel-animation nil
					    (animation :duration 100 :easing-fn 'easing:linear :to 1.05 :target surface :property 'scale-x)
					    (animation :duration 100 :easing-fn 'easing:linear :to 1.05 :target surface :property 'scale-y))
			(parallel-animation nil
					    (animation :duration 100 :easing-fn 'easing:linear :to 1.0 :target surface :property 'scale-x)
					    (animation :duration 100 :easing-fn 'easing:linear :to 1.0 :target surface :property 'scale-y))))

(defmethod mouse-button-handler ((mode desktop-mode) time button state)
  ;; 1. Change (possibly) the active surface
  (when (and (= button #x110) (= state 1) (= 0 (mods-depressed *compositor*)))
    (let ((surface (surface-under-pointer (pointer-x *compositor*) (pointer-y *compositor*) (view mode))))
      ;; When we click on a client which isn't the first client
      (when (and surface (not (equalp surface (active-surface (view mode)))) (not (cursor? surface)))
	(start-animation (pulse-animation surface) :finished-fn (lambda ()
								  (setf (origin-x surface) 0.0)
								  (setf (origin-y surface) 0.0))))
      (activate-surface surface (view mode))
      (when (and surface (not (cursor? surface)))
	(raise-surface surface (view mode))
	(setf (render-needed *compositor*) t))))
  
  ;; Drag window
  (when (and (= button #x110) (= state 1) (= Gui (mods-depressed *compositor*)))
    (let ((surface (surface-under-pointer (pointer-x *compositor*) (pointer-y *compositor*) (view mode))))
      (when surface
	(setf (moving-surface *compositor*) ;;surface))))
	      (make-move-op :surface surface
			    :surface-x (x surface)
			    :surface-y (y surface)
			    :pointer-x (pointer-x *compositor*)
			    :pointer-y (pointer-y *compositor*))))))
	      
  ;; stop drag
  (when (and (moving-surface *compositor*) (= button #x110) (= state 0))
    (setf (moving-surface *compositor*) nil))

  ;; Resize window
  (when (and (= button #x110) (= state 1) (= (+ Gui Shift) (mods-depressed *compositor*)))
    (let ((surface (surface-under-pointer (pointer-x *compositor*) (pointer-y *compositor*) (view mode))))
      (when (and surface (->xdg-surface surface))
	(let ((width (if (input-region surface)
			 (width (first (last (rects (input-region surface)))))
			 (width surface)))
	      (height (if (input-region surface)
			  (height (first (last (rects (input-region surface)))))
			  (height surface))))
	  (setf (resizing-surface *compositor*) (make-resize-op :surface surface
								:pointer-x (pointer-x *compositor*)
								:pointer-y (pointer-y *compositor*)
								:surface-width width
								:surface-height height))))))

  (when (and (resizing-surface *compositor*) (= button #x110) (= state 0))
    (setf (resizing-surface *compositor*) nil))
  
  ;; 2. Send active surface mouse button
  (when (surface-under-pointer (pointer-x *compositor*)
			       (pointer-y *compositor*)
			       (view mode)) 
    (let ((surface (surface-under-pointer (pointer-x *compositor*)
			       (pointer-y *compositor*)
			       (view mode)) ))
      (when (accepts-pointer-events? surface)
	(wl-pointer-send-button (->pointer (client surface))
				0
				time
				button
				state)))))

(defkeybinding (:pressed 16 Ctrl Shift) (mode) (desktop-mode)
  (uiop:quit))

(defkeybinding (:pressed 20 Ctrl Shift) (mode) (desktop-mode)
  (run-program "/usr/bin/weston-terminal"))

#|
(defmousebinding (:pressed 1) (mode) (desktop-mode)
		 (show-menu 'root-menu))
|#

(defkeybinding (:pressed 15 Gui) (mode) (desktop-mode)
  (push-mode (view mode) (make-instance 'alt-tab-mode)))

(defkeybinding (:pressed 106 Gui) (mode) (desktop-mode)
  (with-slots (view) mode
    (let* ((views (views *compositor*))
	   (count (length views))
	   (pos (position view views)))
      (when (not (= pos (- count 1)))
	(setf (current-view *compositor*) (nth (+ pos 1) views)))))
  (setf (render-needed *compositor*) t))

(defkeybinding (:pressed 105 Gui) (mode) (desktop-mode)
  (with-slots (view) mode
    (let* ((views (views *compositor*))
	   (count (length views))
	   (pos (position view views)))
      (when (not (= pos 0))
	(setf (current-view *compositor*) (nth (- pos 1) views)))))
  (setf (render-needed *compositor*) t))

(defmethod first-commit ((mode desktop-mode) surface)
  (when (and (> (width surface) 0) (> (height surface) 0))
    (let ((animation (sequential-animation
		      (lambda ()
			(setf (origin-x surface) 0.0)
			(setf (origin-y surface) 0.0))
		      (animation :target surface
			       :property 'scale-x
			       :easing-fn 'easing:out-exp
			       :from 0
			       :to 1.0
			       :duration 500)
		      (animation :target surface
				 :property 'scale-y
				 :easing-fn 'easing:out-exp
				 :to 1.0
				 :duration 500))))
      (setf (origin-x surface) (/ (width surface) 2))
      (setf (origin-y surface) (/ (height surface) 2))
      (setf (scale-y surface) (/ 6 (height surface)))
      (start-animation animation))))

(defun-g desktop-mode-vertex-shader ((vert g-pt) &uniform (ortho :mat4) (origin :mat4) (origin-inverse :mat4) (surface-scale :mat4) (surface-translate :mat4))
  (values (* ortho surface-translate origin-inverse surface-scale origin (v! (pos vert) 1))
	  (:smooth (tex vert))))

(def-g-> mapping-pipeline ()
  #'desktop-mode-vertex-shader #'default-fragment-shader)

(defmethod render ((mode desktop-mode) &optional view-fbo)
  (apply #'gl:clear-color (clear-color mode))
  (when view-fbo
    (cepl:clear view-fbo))
  (mapcar (lambda (surface)
	    (when (and (texture surface) (not (cursor? surface)))
	      (with-blending (blending-parameters mode)
		(with-rect (vertex-stream (width surface) (height surface))
		  (let ((texture (texture-of surface)))
		    (gl:viewport 0 0 (screen-width *compositor*) (screen-height *compositor*))
		    (map-g-default/fbo view-fbo #'mapping-pipeline vertex-stream
				       :ortho (projection mode)
				       :origin (m4:translation (v! (- (origin-x surface)) (- (origin-y surface)) 0))
				       :origin-inverse (m4:translation (v! (origin-x surface) (origin-y surface) 0))
				       :surface-scale (m4:scale (v! (scale-x surface) (scale-y surface) 1.0))
				       :surface-translate (m4:translation (v! (x surface) (y surface) 0.0))
				       :texture texture
				       :alpha (opacity surface)))))))
	  (reverse (surfaces (view mode)))))
