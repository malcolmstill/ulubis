
(in-package :ulubis)

(defgeneric keyboard-handler (object time keycode keysym state))

(defmethod keyboard-handler ((o (eql nil)) time keycode keysym state))

(defgeneric cancel-mods (surface))

(defmethod cancel-mods ((object (eql nil)))
  nil)
