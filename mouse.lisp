
(in-package :ulubis)

#|
Generic methods and standard methods for mouse handling
|#

(defgeneric mouse-motion-handler (object time delta-x delta-y))

(defmethod mouse-motion-handler ((object (eql nil)) time delta-x delta-y)
  nil)

(defgeneric mouse-button-handler (object time button state))

(defmethod mouse-button-handler ((object (eql nil)) time button state)
  nil)
