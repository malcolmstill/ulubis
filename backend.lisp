
(defpackage :ulubis-backend
  (:use :common-lisp :cffi :xkb)
  (:export
   backend-name
   initialise-backend
   process-events
   register-keyboard-handler
   register-mouse-motion-handler
   register-mouse-button-handler
   register-window-event-handler
   swap-buffers
   destroy-backend))

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
