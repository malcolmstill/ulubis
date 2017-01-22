
(in-package :ulubis)

(def-wl-callback start-drag (client data-device (source :pointer) (origin :pointer) (icon :pointer) (serial :uint32))
  )

(def-wl-callback set-selection (client data-device (source :pointer) (serial :uint32))
  )

(defimplementation wl-data-device ()
  ((:start-drag start-drag)
   (:set-selection set-selection))
  ())
  
