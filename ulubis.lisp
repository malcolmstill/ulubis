
(in-package :ulubis)

(defparameter *compositor* nil)

#|
Smooth animation with
(defun main-loop-drm (event-loop)
  (let ((wayland-fd (wl-event-loop-get-fd event-loop))
	(backend-fd (get-fd (backend *compositor*))))
    (nix:with-pollfds (pollfds
		       (wayland-pollfd wayland-fd nix:pollin nix:pollpri)
		       (backend-pollfd backend-fd nix:pollin nix:pollpri))
      (loop :while (running *compositor*)
	 :do (progn
	       (wl-event-loop-dispatch event-loop 0)
	       (wl-display-flush-clients (display *compositor*))
	       (animation::update-animations (lambda ()
					       (setf (render-needed *compositor*) t)))
	       (let ((event (nix:poll pollfds 2 5)))
		 (when (render-needed *compositor*)
		   (render (current-mode))
		   (swap-buffers (backend *compositor*))
		   (setf (render-needed *compositor*) nil))
		 (when event
		   (when (= (nix:poll-return-event wayland-pollfd) nix:pollin)
		     )
		   
		   (when (= (nix:poll-return-event backend-pollfd) nix:pollin)
		     (process-events (backend *compositor*))))))))))
|#


(defun main-loop-drm (event-loop)
  (let ((wayland-fd (wl-event-loop-get-fd event-loop))
	(backend-fd (get-fd (backend *compositor*))))
    (nix:with-pollfds (pollfds
		       (wayland-pollfd wayland-fd nix:pollin nix:pollpri)
		       (backend-pollfd backend-fd nix:pollin nix:pollpri))
      (initialize-animation event-loop)
      (loop :while (running *compositor*)
	 :do (progn
	       (when (render-needed *compositor*)
		 (render (current-mode))
		 (swap-buffers (backend *compositor*))
		 (setf (render-needed *compositor*) nil))
	       (wl-event-loop-dispatch event-loop 0)
	       (wl-display-flush-clients (display *compositor*))
	       (alexandria:ignore-some-conditions (nix:eintr)
		 (let ((event (nix:poll pollfds 2 -1)))
		   (wl-event-loop-dispatch event-loop 0)
		   (wl-display-flush-clients (display *compositor*))
		   (animation::update-animations (lambda ()
						   (setf (render-needed *compositor*) t)))
		   (when event
		     (when (= (nix:poll-return-event backend-pollfd) nix:pollin)
		       (process-events (backend *compositor*)))))))))))

#|
(defun main-loop-sdl (event-loop)
  (if (running *compositor*)
       (progn
	 (wl-event-loop-dispatch event-loop 0)
	 (wl-display-flush-clients (display *compositor*))
	 (process-events (backend *compositor*))
	 (animation::update-animations (lambda ()
					 (setf (render-needed *compositor*) t)))
	 (when (render-needed *compositor*)
	   (render (current-mode))
	   (swap-buffers (backend *compositor*))
	   (setf (render-needed *compositor*) nil))
	 (main-loop-sdl event-loop))
       nil))
|#

(defun main-loop-sdl (event-loop)
  (let ((wayland-fd (wl-event-loop-get-fd event-loop)))
    (nix:with-pollfds (pollfds
		       (wayland-pollfd wayland-fd nix:pollin nix:pollpri))
      (initialize-animation event-loop)
      (loop :while (running *compositor*)
	 :do (progn
	       (when (render-needed *compositor*)
		 (render (current-mode))
		 (swap-buffers (backend *compositor*))
		 (setf (render-needed *compositor*) nil))
	       (wl-event-loop-dispatch event-loop 0)
	       (wl-display-flush-clients (display *compositor*))
	       (alexandria:ignore-some-conditions (nix:eintr)
		 (let ((event (nix:poll pollfds 1 5)))
		   (wl-event-loop-dispatch event-loop 0)
		   (wl-display-flush-clients (display *compositor*))
		   (animation::update-animations (lambda ()
						   (setf (render-needed *compositor*) t)))
		   (process-events (backend *compositor*)))))))))
 
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
				      (round (+ width delta-x))
				      (round (+ height delta-y))
				      array
				      (get-milliseconds))
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
				    (round width)
				    (round height)
				    array
				    (get-milliseconds))
	(wl-array-release array)
	(foreign-free array)))))

(defun deactivate-surface (surface)
  (when surface
    (when (->keyboard (client surface))
      (wl-keyboard-send-leave (->keyboard (client surface)) 0 (->surface surface)))
    (when (->xdg-surface surface)
      (let ((array (foreign-alloc '(:struct wl_array))))
	(wl-array-init array)
	(xdg-surface-send-configure (->xdg-surface surface) 0 0 array (get-milliseconds))
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
	   (xdg-surface-send-configure (->xdg-surface surface) 0 0 array (get-milliseconds)))
	 (wl-array-release array)
	 (foreign-free array))))))

(defun call-mouse-motion-handler (time x y)
  (when (show-cursor *compositor*)
    (setf (render-needed *compositor*) t))
  (mouse-motion-handler (current-mode) time x y))

;; Should be able to have "active" window without raising (focus follows mouse)
(defun call-mouse-button-handler (time button state)
  (mouse-button-handler (current-mode) time button state))

(defun window-event-handler ()
  (new-xkb-state *compositor*)
  (setf (render-needed *compositor*) t))

(defun call-keyboard-handler (time key state)
  ;;(format t "call-keyboard-handler ~A ~A ~A~%" key state mods)
  (xkb:xkb-state-update-key (xkb-state *compositor*) (+ key 8) state)
  (setf (mods-depressed *compositor*) (xkb:xkb-state-serialize-mods
				       (xkb-state *compositor*) 1))
  (setf (mods-latched *compositor*) (xkb:xkb-state-serialize-mods
				       (xkb-state *compositor*) 2))
  (setf (mods-locked *compositor*) (xkb:xkb-state-serialize-mods
				       (xkb-state *compositor*) 4))
  (setf (mods-group *compositor*) (xkb:xkb-state-serialize-layout
				       (xkb-state *compositor*) 64))
  (when (and (numberp key) (numberp state))
    (keyboard-handler (current-mode) time key state)))

(defun initialise ()
  (unwind-protect
       (progn
	 #+sbcl
	 (sb-int:set-floating-point-modes :traps nil)
	 
	 ;; Make our compositor class
	 (setf *compositor* (make-instance 'compositor))
	 (setf (render-fn *compositor*) 'render)
	 (format t "Made compositor object~%")

	 (when (probe-file "~/.ulubis.lisp")
	   (load "~/.ulubis.lisp"))
	 
	 ;; Initialise backend
	 (format t "Initialising backend: ~A~%" backend-name)
	 (setf (backend *compositor*) (make-instance 'backend))
	 (initialise-backend (backend *compositor*)
			     (screen-width *compositor*)
			     (screen-height *compositor*)
			     (devices *compositor*))
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

;;	 (format t "Initializing wayland~%")
;;	 (initialise-wayland) ;; plumbing.lisp
;;	 (format t "Initializing device manager~%")
	 ;;	 (init-device-manager) ;; plumbing.lisp
	 (initialize-wayland-server-interfaces) 
	 (initialize-xdg-shell-server-interfaces) 
	 (set-implementations) ;; plumbing-unwrapped.lisp
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
	 (format t "Making xdg-shell-server interfaces~%")
;;	 (make-xdg-shell-server-interfaces)
	 ;;(make-xdg-interfaces)
	 (wl-global-create (display *compositor*)
			   xdg-shell-interface
			   1
			   (null-pointer)
			   (callback xdg-shell-bind))
	 (wl-global-create (display *compositor*)
			   wl-seat-interface
			   4
			   (null-pointer)
			   (callback seat-bind))

	 (wl-global-create (display *compositor*)
			   wl-data-device-manager-interface
			   3
			   (null-pointer)
			   (callback device-manager-bind))

;;	 (init-wl-output) ;; plumbing.lisp
	   (wl-global-create (display *compositor*) ;; plumbing-unwrapped.lisp
		    wl-output-interface
		    2
		    (null-pointer)
		    (callback output-bind))
	 
	 ;; Initialise shared memory
	 (wl-display-init-shm (display *compositor*))
	 ;; Run main loop
	 (format t "Running main loop~%")
	 (setf (running *compositor*) t)
	 (if (string-equal (symbol-name backend-name) "backend-drm-gbm")
	     (main-loop-drm (wl-display-get-event-loop (display *compositor*)))
	     (main-loop-sdl (wl-display-get-event-loop (display *compositor*)))))
    (when (display *compositor*)
      (wl-display-destroy (display *compositor*))
      (setf (display *compositor*) nil))
    (destroy-backend (backend *compositor*))
    (setf *compositor* nil)
    (format t "Exit compositor")))

(defun run-compositor ()
  (initialise))
