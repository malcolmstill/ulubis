
(defpackage :ulubis-backend
  (:use :common-lisp :cffi :xkb)
  (:export
   backend
   backend-name
   initialise-backend
   process-events
   get-fd
   register-keyboard-handler
   register-mouse-motion-handler
   register-mouse-button-handler
   register-window-event-handler
   swap-buffers
   destroy-backend
   event-loop-add-drm-fd
   set-scheduled
   get-scheduled
   init-egl
   egl-supported?
   egl-surface?
   egl-get-dimensions
   egl-texture-from-image
   ))

(in-package :ulubis-backend)

(defparameter backend-name nil)

(defgeneric initialise-backend (backend width height devices))
(defgeneric process-events (backend))
(defgeneric register-keyboard-handler (backend keyboard-handler))
(defgeneric register-mouse-motion-handler (backend mouse-motion-handler))
(defgeneric register-mouse-button-handler (backend mouse-button-handler))
(defgeneric register-window-event-handler (backend keyboard-handler)) ;; Useful if running on X
(defgeneric swap-buffers (backend))
(defgeneric destroy-backend (backend))
(defgeneric init-egl (backend wl-display))
(defgeneric egl-supported? (backend))
(defgeneric egl-surface? (backend buffer))
(defgeneric egl-get-dimensions (backend buffer))
(defgeneric egl-texture-from-image (backend buffer width height))

;; DRM backend only
(defgeneric event-loop-add-drm-fd (backend event-loop))
(defgeneric set-scheduled (backend value))
(defgeneric get-scheduled (backend))
