
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
   get-keymap
   ;;create-texture
   ;;destroy-texture
   ;;render
   swap-buffers
   destroy-backend
   state
   keymap
   xkb-context))

(in-package :ulubis-backend)

(defparameter backend-name nil)

(defgeneric intialise-backend (backend))
(defgeneric process-events (backend))
(defgeneric register-keyboard-handler (backend keyboard-handler))
(defgeneric register-mouse-motion-handler (backend mouse-motion-handler))
(defgeneric register-mouse-button-handler (backend mouse-button-handler))
(defgeneric register-window-event-handler (backend keyboard-handler)) ;; Useful if running on X
(defgeneric get-keymap (backend))
;;(defgeneric create-texture (backend client))
;;(defgeneric destroy-texture (backend client))
;;(defgeneric render (backend compositor))
(defgeneric swap-buffers (backend))
(defgeneric destroy-backend (backend))
