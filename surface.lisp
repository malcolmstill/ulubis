
(in-package :ulubis)

(defmethod keyboard-handler ((surface isurface) time keycode keysym state)
  (when (and keycode (keyboard (client surface)))
    (wl-keyboard-send-key (->resource (keyboard (client surface))) 0 time keycode state))
  (when (and (keyboard (client surface)))
    (wl-keyboard-send-modifiers (->resource (keyboard (client surface))) 0
				(mods-depressed *compositor*)
				(mods-latched *compositor*)
				(mods-locked *compositor*)
				(mods-group *compositor*))))

(defmethod cancel-mods ((surface isurface))
  (when (and (keyboard (client surface)))
    (wl-keyboard-send-modifiers (->resource (keyboard (client surface))) 0
				0
				0
				0
				0)))

(defun effective-width (surface)
  (if (input-region (wl-surface surface))
      (width (first (last (rects (input-region (wl-surface surface))))))
      (width (wl-surface surface))))

(defun effective-height (surface)
  (if (input-region (wl-surface surface))
      (height (first (last (rects (input-region (wl-surface surface))))))
      (height (wl-surface surface))))

(defun activate-surface (surface mode)
  (with-slots (view) mode
    (with-slots (active-surface) view
      (setf active-surface
	    (activate surface active-surface
		      (list (mods-depressed *compositor*)
			    (mods-latched *compositor*)
			    (mods-locked *compositor*)
			    (mods-group *compositor*)))))))

(defun move-surface (x y move-op)
  "Move surface to location X and Y given the MOVE-OP"
  (let ((surface (move-op-surface move-op)))
    (setf (x surface) (round (+ (move-op-surface-x move-op) (- x (move-op-pointer-x move-op)))))
    (setf (y surface) (round (+ (move-op-surface-y move-op) (- y (move-op-pointer-y move-op)))))
    (setf (render-needed *compositor*) t)))

(defun resize-surface (x y view resize-op)
  "Resize surface given new pointer location (X,Y) and saved information in RESIZE-OP"
  (let* ((surface (resize-op-surface resize-op))
	 (saved-width (resize-op-surface-width resize-op))
	 (saved-height (resize-op-surface-height resize-op))
	 (saved-pointer-x (resize-op-pointer-x resize-op))
	 (saved-pointer-y (resize-op-pointer-y resize-op))
	 (delta-x (- x saved-pointer-x))
	 (delta-y (- y saved-pointer-y)))
    (case (resize-op-direction resize-op)
      (2 (resize-surface-absolute surface
				  view
				  saved-width
				  (+ saved-height delta-y)))
      (8 (resize-surface-absolute surface
				  view
				  (+ saved-width delta-x)
				  saved-height))
      (10 (resize-surface-absolute surface
				   view
				   (+ saved-width delta-x)
				   (+ saved-height delta-y)))
      (t nil))))

(defun resize-surface-absolute (surface view width height)
  (when (> width 32) (> height 32)
    (if (equalp surface (active-surface view))
	(resize surface width height (get-milliseconds) :activate? t)
	(resize surface width height (get-milliseconds) :activate? nil))))

(defun send-surface-pointer-motion (x y time surface)
  (when (and surface (pointer (client surface)))
    (wl-pointer-send-motion (->resource (pointer (client surface)))
			    time
			    (round (* 256 (- x (x surface))))
			    (round (* 256 (- y (y surface)))))
    ;; Need to check client handles version 5
    ;;(wl-pointer-send-frame (waylisp:->pointer (waylisp:client surface)))
    ))

(defmethod send-leave ((nothing (eql nil)))
  nil)

(defmethod send-leave ((surface isurface))
  (when (and (client surface) (pointer (client surface)))
    (wl-pointer-send-leave (->resource (pointer (client surface)))
			   0
			   (->resource (wl-surface surface)))))

(defmethod send-enter ((nothing (eql nil)) x y)
  nil)

(defmethod send-enter ((surface isurface) x y)
  (when (and (client surface) (pointer (client surface)))
    (wl-pointer-send-enter (->resource (pointer (client surface)))
			   0
			   (->resource (wl-surface surface))
			   (round (* 256 (- x (x surface))))
			   (round (* 256 (- y (y surface)))))))

(defmethod send-button ((nothing (eql nil)) time button state)
  nil)

(defmethod send-button ((surface isurface) time button state)
  (when (and (client surface) (pointer (client surfacE)))
    (wl-pointer-send-button (->resource (pointer (client surface)))
			    0
			    time
			    button
			    state)))
