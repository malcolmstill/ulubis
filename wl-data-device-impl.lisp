
(in-package :ulubis)

(def-wl-callback start-drag (client resource (source :pointer) (origin :pointer) (icon :pointer) (serial :uint32))
  )

(def-wl-callback set-selection (client resource (source :pointer) (serial :uint32))
  )

(defimplementation wl-data-device ()
  ((:start-drag start-drag)
   (:set-selection set-selection))
  ())
  
