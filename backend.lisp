
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
   set-draw-function
   swap-buffers
   destroy-backend
   event-loop-add-drm-fd
   set-scheduled
   get-scheduled))

(in-package :ulubis-backend)

(defparameter backend-name nil)

(defgeneric intialise-backend (backend width height devices))
(defgeneric process-events (backend))
(defgeneric register-keyboard-handler (backend keyboard-handler))
(defgeneric register-mouse-motion-handler (backend mouse-motion-handler))
(defgeneric register-mouse-button-handler (backend mouse-button-handler))
(defgeneric register-window-event-handler (backend keyboard-handler)) ;; Useful if running on X
(defgeneric swap-buffers (backend))
(defgeneric destroy-backend (backend))

;; DRM backend only
(defgeneric event-loop-add-drm-fd (backend event-loop))
(defgeneric set-scheduled (backend value))
(defgeneric get-scheduled (backend))
(defgeneric set-draw-function (backend draw-fn))
