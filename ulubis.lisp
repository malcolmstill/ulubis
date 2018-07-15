
(in-package :ulubis)

(defparameter *compositor* nil)

#|
We currently just ask the current-view to render itself.
This does not allow animation. Instead, if on the compositor
level (when we support multiple monitors it will be on the level
of the desktop...the compositor will have a list of desktops),
we also have the concept of modes, we can ask the current mode
to render itself. That mode will have the virtual desktops (views)
as "surfaces" and can render them as it pleases.

Instead of (texture-of (current-view *compositor*))
we'll want (texture-of (current-mode *compositor*))

At the moment only views have fbos.
Scratch that, effects also have fbos. But let's ignore effects
for the moment.

In the way that we have fbos on view we will have fbos on desktops.
Or are fbos on desktops just the default fbo that we have from our
GL context? I think the latter...we don't need an explicit fbo.

However, I think we want our views to also behave like surfaces.
I.e. they have x,y position widht height (albeit that of the screen).

We currently define isurface within waylisp. I can't remember exactly why
it's defined in there rather than in ulubis itself. Maybe I should think about
moving it back. If views are also isurfaces that should be within ulubis.
|#
(defun draw-screen ()
  (with-screen (vs)
    (gl:clear-color 0.3 0.3 0.3 0.0)
    (cepl:clear)
    ;; We are just rendering into the default fbo
    (render (current-mode (screen *compositor*)))
    (gl:enable :blend)
    (draw-cursor (cursor-surface *compositor*)
		 nil
		 (pointer-x *compositor*)
		 (pointer-y *compositor*)
		 (ortho 0 (screen-width *compositor*) (screen-height *compositor*) 0 1 -1))
    (swap-buffers (backend *compositor*))
    (setf (render-needed *compositor*) nil)
    (loop :for callback :in (callbacks *compositor*) :do
	 (when (find (client callback) waylisp::*clients*)
	   ;; We can end up getting a frame request after the client has been deleted
	   ;; if we try and send-done or destroy we will get a memory fault
	   (wl-callback-send-done (->resource callback) (get-milliseconds))
	   (wl-resource-destroy (->resource callback)))
	 (remove-resource callback))
    (setf (callbacks *compositor*) nil)))

(defcallback input-callback :void ((fd :int) (mask :int) (data :pointer))
  (process-events (backend *compositor*)))

(defun main-loop-drm (event-loop)
  (let ((libinput-fd (get-fd (backend *compositor*))))
    (initialize-animation event-loop)
    (wl-event-loop-add-fd event-loop libinput-fd 1 (callback input-callback) (null-pointer))
    (event-loop-add-drm-fd (backend *compositor*) event-loop)
    (loop :while (running *compositor*)
       :do (progn
	     (when (and (render-needed *compositor*) (not (get-scheduled (backend *compositor*))))
	       (draw-screen))
	     (wl-display-flush-clients (display *compositor*))
	     (wl-event-loop-dispatch event-loop -1)
	     (animation::update-animations (lambda () (setf (render-needed *compositor*) t)))))))

(defun main-loop-sdl (event-loop)
  (let ((wayland-fd (wl-event-loop-get-fd event-loop)))
    (syscall:with-pollfds (pollfds
			   (wayland-pollfd wayland-fd syscall:pollin syscall:pollpri))
      (initialize-animation event-loop)
      (loop :while (running *compositor*)
	 :do (progn
	       (when (render-needed *compositor*)
		 (draw-screen))
	       (wl-event-loop-dispatch event-loop 0)
	       (wl-display-flush-clients (display *compositor*))
	       (alexandria:ignore-some-conditions (nix:eintr)
		 (let ((event (syscall:poll pollfds 1 5)))
		   (wl-event-loop-dispatch event-loop 0)
		   (wl-display-flush-clients (display *compositor*))
		   (animation::update-animations (lambda ()
						   (setf (render-needed *compositor*) t)))
		   (process-events (backend *compositor*)))))))))

(defun call-mouse-motion-handler (time x y)
  (when (show-cursor *compositor*)
    (setf (render-needed *compositor*) t))
  (mouse-motion-handler (screen *compositor*) time x y))

;; Should be able to have "active" window without raising (focus follows mouse)
(defun call-mouse-button-handler (time button state)
  (mouse-button-handler (screen *compositor*) time button state))

(defun window-event-handler ()
  (new-xkb-state *compositor*)
  (setf (render-needed *compositor*) t))

;; Kludge for SDL backend on multi-desktop WMs to avoid sticky mods.
;; Seems like sway uses wl_keyboard_listener. We probably should use
;; it too instead of the whole thing.
(defvar *depressed-keys* (list))

(defun call-keyboard-handler (time keycode state)
  (let ((keysym (xkb:xkb-state-key-get-one-sym (xkb-state *compositor*) (+ keycode 8))))
    (if (and (= state 1)
	     (member keycode *depressed-keys*))
	(progn
	  ;;(format t "!!! Not updating key state~%")
	  )
	(xkb:xkb-state-update-key (xkb-state *compositor*) (+ keycode 8) state))
    (if (= state 1)
	(push keycode *depressed-keys*)
	(setf *depressed-keys* (delete keycode *depressed-keys*)))
    (setf (mods-depressed *compositor*) (xkb:xkb-state-serialize-mods (xkb-state *compositor*) 1))
    (setf (mods-latched *compositor*) (xkb:xkb-state-serialize-mods (xkb-state *compositor*) 2))
    (setf (mods-locked *compositor*) (xkb:xkb-state-serialize-mods (xkb-state *compositor*) 4))
    (setf (mods-group *compositor*) (xkb:xkb-state-serialize-layout (xkb-state *compositor*) 64))
    (when (and (numberp keysym) (numberp state))
      (keyboard-handler (screen *compositor*)
			time
			keycode
			keysym
			state))))

(defun initialise ()
  (unwind-protect
       (block main-handler
	 (handler-bind ((error #'(lambda (e)
				   (format t "~%Oops! Something went wrong with ulubis...we throw ourselves at your mercy! Exiting wih error:~%")
				   (trivial-backtrace:print-backtrace e)
				   (return-from main-handler))))
	 #+sbcl
	 (sb-int:set-floating-point-modes :traps nil)
	 
	 ;; Make our compositor class
	 (setf *compositor* (make-instance 'compositor))
	 
	 (when (probe-file "~/.ulubis.lisp")
	   (load "~/.ulubis.lisp"))
	 
	 ;; Initialise backend
	 (setf (backend *compositor*) (make-instance 'backend))
	 (initialise-backend (backend *compositor*)
			     (screen-width *compositor*)
			     (screen-height *compositor*)
			     (devices *compositor*))

	 ;; ulubis will attempt to run the function STARTUP
	 ;; This should be defined in the user's ~/.ulubis.lisp
	 ;; And is intended to set up things like the number
	 ;; of virtual desktops (views), etc.
	 (handler-case (startup)
	   (undefined-function ()
	     (progn
	       (make-screen 'virtual-desktop-mode)
	       (push-view 'desktop-mode))
	     (setf (active-surface (screen *compositor*)) (first (surfaces (screen *compositor*))))))
	 
	 (register-mouse-motion-handler (backend *compositor*) 'call-mouse-motion-handler)
	 (register-mouse-button-handler (backend *compositor*) 'call-mouse-button-handler)
	 (register-window-event-handler (backend *compositor*) 'window-event-handler)
	 (register-keyboard-handler (backend *compositor*) 'call-keyboard-handler)
	 
	 ;; Create our wayland display
	 (setf (display *compositor*) (wl-display-create))
	 (format t "Opened socket: ~A~%" (wl-display-add-socket-auto (display *compositor*)))

	 ;; Initialise shared memory

	 
	 (initialize-wayland-server-interfaces) 
	 (initialize-xdg-shell-server-interfaces)
	 (initialize-zxdg-shell-v6-server-interfaces) 
	 ;;(set-implementations)
	 (set-implementation-wl-surface)
	 (set-implementation-wl-seat)
	 (set-implementation-wl-pointer)
	 (set-implementation-wl-seat)
	 ;;(set-implementation-wl-callback)
	 (set-implementation-wl-region)
	 (set-implementation-wl-compositor)
	 (set-implementation-wl-subcompositor)
	 (set-implementation-wl-subsurface)
	 (set-implementation-wl-output)	 
	 (set-implementation-wl-shell)
	 (set-implementation-wl-shell-surface)
	 (set-implementation-wl-data-device-manager)
	 (set-implementation-wl-data-device)
	 (set-implementation-wl-data-source)
	 (set-implementation-zxdg-shell-v6)
	 (set-implementation-zxdg-surface-v6)
	 (set-implementation-zxdg-toplevel-v6)
	 (set-implementation-zxdg-positioner-v6)
	 (set-implementation-zxdg-popup-v6)
	 (set-implementation-xdg-shell)
	 (set-implementation-xdg-surface)

	 (wl-display-init-shm (display *compositor*))

	 (wl-global-create (display *compositor*) 
			   wl-output-interface
			   2
			   (null-pointer)
			   (callback output-bind))

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
			   wl-seat-interface
			   3
			   (null-pointer)
			   (callback seat-bind))

	 (wl-global-create (display *compositor*)
			   wl-data-device-manager-interface
			   3
			   (null-pointer)
			   (callback device-manager-bind))

	 (wl-global-create (display *compositor*) 
			   wl-subcompositor-interface
			   1
			   (null-pointer)
			   (callback subcompositor-bind))
	 
	 (wl-global-create (display *compositor*)
			   zxdg-shell-v6-interface
			   1
			   (null-pointer)
			   (callback zxdg-shell-v6-bind))
	 
	 (wl-global-create (display *compositor*)
			   xdg-shell-interface
			   1
			   (null-pointer)
			   (callback xdg-shell-bind))

	 ;; Run main loop
	 ;; (format t "Running main loop~%")
	 (setf (running *compositor*) t)
	 (if (string-equal (symbol-name backend-name) "backend-drm-gbm")
	     (main-loop-drm (wl-display-get-event-loop (display *compositor*)))
	     (main-loop-sdl (wl-display-get-event-loop (display *compositor*))))))
    (when (display *compositor*)
      (wl-display-destroy (display *compositor*))
      (setf (display *compositor*) nil))
    (destroy-backend (backend *compositor*))
    (setf *compositor* nil)))
  
(defun run-compositor ()
  (initialise))
