
(in-package :ulubis)

(defparameter *default-mode* nil)

(defclass default-mode (mode)
  ((clear-color :accessor clear-color :initarg :clear-color :initform (list 0.3 0.3 0.3 0.0))
   (projection :accessor projection :initarg :projection :initform (m4:identity))
   (focus-follows-mouse :accessor focus-follows-mouse :initarg :focus-follows-mouse :initform nil)))

(defmethod init-mode ((mode default-mode))
  (setf (projection mode) (ortho 0 (screen-width *compositor*) (screen-height *compositor*) 0 1 -1)))

(defmethod mouse-motion-handler ((mode default-mode) x y)
  (let ((delta-x (- x (pointer-x *compositor*)))
	(delta-y (- y (pointer-y *compositor*))))
    ;; Update the pointer location
    (setf (pointer-x *compositor*) x)
    (setf (pointer-y *compositor*) y)
    (when (cursor-surface *compositor*)
      (setf (render-needed *compositor*) t))
    (cond
      ((moving-surface *compositor*) ;; 1. If we are dragging a window...
       (progn
	 (incf (x (moving-surface *compositor*)) delta-x) ;;... update its location
	 (incf (y (moving-surface *compositor*)) delta-y)
	 (setf (render-needed *compositor*) t)))
      ((resizing-surface *compositor*)
       (let* ((saved-width (resize-op-surface-width (resizing-surface *compositor*)))
	      (saved-height (resize-op-surface-height (resizing-surface *compositor*)))
	      (saved-pointer-x (resize-op-pointer-x (resizing-surface *compositor*)))
	      (saved-pointer-y (resize-op-pointer-y (resizing-surface *compositor*)))
	      (delta-x (- x saved-pointer-x))
	      (delta-y (- y saved-pointer-y)))
	 (resize-surface-absolute (resize-op-surface (resizing-surface *compositor*)) (+ saved-width delta-x) (+ saved-height delta-y))))
      ;; 2. The pointer has left the current surface
      ((not (equalp (pointer-surface *compositor*) (surface-under-pointer x y *compositor*)))
       (progn
	 (setf (cursor-surface *compositor*) nil)
	 (when (focus-follows-mouse mode)
	   (deactivate-surface (pointer-surface *compositor*)))
	 (when (accepts-pointer-events? (pointer-surface *compositor*))
	   (wl-pointer-send-leave (->pointer (client (pointer-surface *compositor*)))
				  0
				  (->surface (pointer-surface *compositor*))))
	 (setf (pointer-surface *compositor*) (surface-under-pointer x y *compositor*))
	 (when (focus-follows-mouse mode)
	     (activate-surface (pointer-surface *compositor*)))
	 (when (accepts-pointer-events? (pointer-surface *compositor*))
	   (wl-pointer-send-enter (->pointer (client (pointer-surface *compositor*)))
				  0
				  (->surface (pointer-surface *compositor*))
				  (* 256 (- x (x (pointer-surface *compositor*))))
				  (* 256 (- y (y (pointer-surface *compositor*))))))))
      ;; 3. Pointer is over previous surface
      ((equalp (pointer-surface *compositor*) (surface-under-pointer x y *compositor*))
       (when (accepts-pointer-events? (pointer-surface *compositor*))
	 (wl-pointer-send-motion (->pointer (client (pointer-surface *compositor*)))
				 (get-internal-real-time)
				 (* 256 (- x (x (pointer-surface *compositor*))))
				 (* 256 (- y (y (pointer-surface *compositor*)))))
	 (wl-pointer-send-frame (->pointer (client (pointer-surface *compositor*))))
	 )))))

(defmethod mouse-button-handler ((mode default-mode) button state)
  ;; 1. Change (possibly) the active surface
  (when (and (= button #x110) (= state 1) (not (= 4 (mods-depressed *compositor*))))
      (let ((surface (surface-under-pointer (pointer-x *compositor*) (pointer-y *compositor*) *compositor*)))
	;; When we click on a client which isn't the first client
	(when (and surface (not (equalp surface (active-surface *compositor*))))
	  (start-animation
	   (make-instance 'sequential-animation
			  :animations (list
				       (make-instance 'parallel-animation 
						      :animations (list
								   (make-instance 'animation :duration 100 :easing-fn 'easing:linear
										  :to 1.05 :target surface :property 'scale-x)
								   (make-instance 'animation :duration 100 :easing-fn 'easing:linear
										  :to 1.05 :target surface :property 'scale-y)))
				       (make-instance 'parallel-animation 
						      :animations (list
								   (make-instance 'animation :duration 100 :easing-fn 'easing:linear
										  :to 1.0 :target surface :property 'scale-x)
								   (make-instance 'animation :duration 100 :easing-fn 'easing:linear
										  :to 1.0 :target surface :property 'scale-y))))))
	  
	  )
	(activate-surface surface)
	(when surface
	  (raise-surface surface *compositor*)
	  (setf (render-needed *compositor*) t))))

  ;; Drag window
  (when (and (= button #x110) (= state 1) (= 4 (mods-depressed *compositor*)))
    (let ((surface (surface-under-pointer (pointer-x *compositor*) (pointer-y *compositor*) *compositor*)))
      (when surface
	(setf (moving-surface *compositor*) surface))))

  ;; stop drag
  (when (and (moving-surface *compositor*) (= button #x110) (= state 0))
    (setf (moving-surface *compositor*) nil))

  ;; Resize window
  (when (and (= button #x110) (= state 1) (= 5 (mods-depressed *compositor*)))
    (let ((surface (surface-under-pointer (pointer-x *compositor*) (pointer-y *compositor*) *compositor*)))
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
								:surface-height height
								))))))

  (when (and (resizing-surface *compositor*) (= button #x110) (= state 0))
    (setf (resizing-surface *compositor*) nil))
  
  ;; 2. Send active surface mouse button
  (when (surface-under-pointer (pointer-x *compositor*)
			       (pointer-y *compositor*)
			       *compositor*) ;(active-surface *compositor*)
    (let ((surface (surface-under-pointer (pointer-x *compositor*)
			       (pointer-y *compositor*)
			       *compositor*) ))
      (when (accepts-pointer-events? surface)
	(wl-pointer-send-button (->pointer (client surface))
				0
				(get-internal-real-time)
				button
				state)))))

(defmethod keyboard-handler ((mode default-mode) key state mods)
  ;; Control tab
  (when (and (= (mods-depressed *compositor*) 4) (= state 1) (= key 15))
    (push-mode (make-instance 'alt-tab-mode))
    (when (and (active-surface *compositor*) (->keyboard (client (active-surface *compositor*))))
      (wl-keyboard-send-modifiers (->keyboard (client (active-surface *compositor*)))
				  0
				  (logxor 4 (mods-depressed *compositor*))
				  (mods-latched *compositor*)
				  (mods-locked *compositor*)
				  (mods-group *compositor*)))
    (return-from keyboard-handler))
  
  (let ((surface (active-surface *compositor*)))
    (when (and surface key (->keyboard (client surface)))
      (wl-keyboard-send-key (->keyboard (client surface)) 0 (get-internal-real-time) key state))
    (when (and surface mods (->keyboard (client surface)))
      (wl-keyboard-send-modifiers (->keyboard (client surface)) 0
				  (mods-depressed *compositor*)
				  (mods-latched *compositor*)
				  (mods-locked *compositor*)
				  (mods-group *compositor*)))))
    
(def-g-> default-pipeline ()
  #'default-vertex-shader #'default-fragment-shader)

(defmethod render ((mode default-mode))
  (apply #'gl:clear-color (clear-color mode))
  (clear)
  (mapcar (lambda (surface)
	    (when (and (texture surface) (not (cursor? surface)))
	      (with-surface (vertex-stream tex mode surface)
		  (map-g #'default-pipeline vertex-stream
			 :ortho (projection mode)
			 :surface-scale (m4:scale (v! (scale-x surface)
						      (scale-y surface)
						      1.0))
			 :surface-translate (m4:translation
					     (v!
					      (+ (x surface) (- (* (width surface) (- (scale-x surface) 1.0) 0.5)))
					      (+ (y surface) (- (* (height surface) (- (scale-y surface) 1.0) 0.5)))
					      0.0))
			 :texture (sample tex)
			 :alpha 1.0))))
	  (reverse (surfaces *compositor*))))
