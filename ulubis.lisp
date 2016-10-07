
(in-package :ulubis)

(defparameter *compositor* nil)

(defun main-loop (event-loop)
  (if (running *compositor*)
      (progn
	(wl-event-loop-dispatch event-loop 16)
	(wl-display-flush-clients (display *compositor*))
	(process-events (backend *compositor*))
	(animation::update-animations (lambda ()
					(setf (render-needed *compositor*) t)))
	(when (render-needed *compositor*)
	  (render (current-mode))
	  (swap-buffers (backend *compositor*))
	  (setf (render-needed *compositor*) nil))
	(main-loop event-loop))
      nil))

(defun resize-surface-relative (surface delta-x delta-y)
  (with-slots (x y ->xdg-surface input-region) surface
    (let ((width (width (first (last (rects input-region)))))
	  (height (height (first (last (rects input-region))))))
      (format t "Width: ~A, height: ~A, new width: ~A, new height: ~A~%" width height (+ width delta-x) (+ height delta-y))
      (when (and ->xdg-surface (> (+ width delta-x) 32) (> (+ height delta-y) 32))
	(let ((array (foreign-alloc '(:struct wl_array))))
	  (wl-array-init array)
	  (setf (mem-aref (wl-array-add array 4) :int32) 3)
	  (when (equalp surface (active-surface *compositor*))
	    (setf (mem-aref (wl-array-add array 4) :int32) 4))
	  (xdg-surface-send-configure (->xdg-surface surface)
				      (+ width delta-x)
				      (+ height delta-y)
				      array
				      (get-internal-real-time))
	  (wl-array-release array)
	  (foreign-free array))))))

(defun resize-surface-absolute (surface width height)
  (with-slots (x y ->xdg-surface input-region) surface
    (when (and ->xdg-surface (> width 32) (> height 32))
      (let ((array (foreign-alloc '(:struct wl_array))))
	(wl-array-init array)
	(setf (mem-aref (wl-array-add array 4) :int32) 3)
	(when (equalp surface (active-surface *compositor*))
	  (setf (mem-aref (wl-array-add array 4) :int32) 4))
	(xdg-surface-send-configure (->xdg-surface surface)
				    width
				    height
				    array
				    (get-internal-real-time))
	(wl-array-release array)
	(foreign-free array)))))

(defun deactivate-surface (surface)
  (when surface
    (when (->keyboard (client surface))
      (wl-keyboard-send-leave (->keyboard (client surface)) 0 (->surface surface)))
    (when (->xdg-surface surface)
      (let ((array (foreign-alloc '(:struct wl_array))))
	(wl-array-init array)
	(xdg-surface-send-configure (->xdg-surface surface) 0 0 array (get-internal-real-time))
	(wl-array-release array)
	(foreign-free array)))))

(defun activate-surface (surface)
  (cond
    ;; No surface to activate
    ((not surface) (progn
		     (format t "No surface to activate~%")
		     (deactivate-surface (active-surface *compositor*))
		     (setf (active-surface *compositor*) nil)))
    ;; Activating a non-active surface
    ((not (equalp surface (active-surface *compositor*)))
     (progn
       (deactivate-surface (active-surface *compositor*))
       (setf (active-surface *compositor*) surface)
       (let ((array (foreign-alloc '(:struct wl_array))))
	 (wl-array-init array)
	 (when (->keyboard (client surface))
	   (wl-keyboard-send-enter (->keyboard (client surface)) 0 (->surface surface) array)
	   (wl-keyboard-send-modifiers (->keyboard (client surface))
				       0
				       (mods-depressed *compositor*)
				       (mods-latched *compositor*)
				       (mods-locked *compositor*)
				       (mods-group *compositor*)))
	 (when (->xdg-surface surface)
	   (setf (mem-aref (wl-array-add array 4) :int32) 4)
	   (xdg-surface-send-configure (->xdg-surface surface) 0 0 array (get-internal-real-time)))
	 (wl-array-release array)
	 (foreign-free array))))))

(defun call-mouse-motion-handler (x y)
  (mouse-motion-handler (current-mode) x y))

;; Should be able to have "active" window without raising (focus follows mouse)
(defun call-mouse-button-handler (button state)
  (mouse-button-handler (current-mode) button state))

(defun window-event-handler ()
  (setf (render-needed *compositor*) t))

(defun call-keyboard-handler (key state mods)
  ;;(format t "call-keyboard-handler ~A ~A ~A~%" key state mods)
  (when mods
    (setf (mods-depressed *compositor*) (first mods))
    (setf (mods-latched *compositor*) (second mods))
    (setf (mods-locked *compositor*) (third mods))
    (setf (mods-group *compositor*) (fourth mods)))
  (when (and (numberp key) (numberp state))
    (keyboard-handler (current-mode) key state mods)))

(defun initialise ()
  (unwind-protect
       (progn
	 #+sbcl
	 (sb-int:set-floating-point-modes :traps nil)
	 
	 (swank-loader:init)
	 (swank:create-server :port 4005
			      :dont-close t)
	 (swank:set-package "ULUBIS")
	 
	 ;; Make our compositor class
	 (setf *compositor* (make-instance 'compositor))
	 (setf (screen-width *compositor*) 1200)
	 (setf (screen-height *compositor*) 800)
	 (setf (render-fn *compositor*) 'render)
	 
	 ;; Initialise SDL2 backend
	 (setf (backend *compositor*) (make-instance backend-name))
	 (format t "Initialising backend~%")
	 (initialise-backend (backend *compositor*) (screen-width *compositor*) (screen-height *compositor*))
	 (format t "Backend initialised~%")

	 ;; Initialise our default mode
	 (setf *default-mode* (make-instance 'default-mode))
	 (init-mode *default-mode*)
	 
	 (register-mouse-motion-handler (backend *compositor*) 'call-mouse-motion-handler)
	 (register-mouse-button-handler (backend *compositor*) 'call-mouse-button-handler)
	 (register-window-event-handler (backend *compositor*) 'window-event-handler)
	 (register-keyboard-handler (backend *compositor*) 'call-keyboard-handler)

	 ;; Create our wayland display
	 (setf (display *compositor*) (wl-display-create))
	 (format t "Opened socket: ~A~%" (wl-display-add-socket-auto (display *compositor*)))

	 (initialise-wayland)
	 (init-device-manager)
	 (wl-global-create (display *compositor*)
			   wl-compositor-interface
			   3
			   (null-pointer)
			   (callback compositor-bind))
	 (wl-global-create (display *compositor*)
			   wl-shell-interface
			   1
			   (null-pointer)
			   (callback shell-bind))
	 (wl-global-create (display *compositor*)
			   xdg-shell-interface
			   1
			   (null-pointer)
			   (callback xdg-shell-bind))
	 (wl-global-create (display *compositor*)
			   wl-seat-interface
			   1
			   (null-pointer)
			   (callback seat-bind))

	 (wl-global-create (display *compositor*)
			   wl-data-device-manager-interface
			   3
			   (null-pointer)
			   (callback device-manager-bind))

	 (init-wl-output)
	 
	 ;; Initialise shared memory
	 (wl-display-init-shm (display *compositor*))
	 ;; Run main loop
	 (format t "Running main loop~%")
	 (setf (running *compositor*) t)
	 (main-loop (wl-display-get-event-loop (display *compositor*))))
    (when (display *compositor*)
      (wl-display-destroy (display *compositor*))
      (setf (display *compositor*) nil)
      (destroy-backend (backend *compositor*))
      (setf *compositor* nil)
      (format t "Exit compositor"))))

(defun run-compositor ()
  (initialise))
