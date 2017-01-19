
(in-package :ulubis)

(def-wl-callback region-add (client region (x :int32) (y :int32) (width :int32) (height :int32))
  (push (make-instance 'waylisp:wl-rect :x x :y y :width width :height height :operation :add) (rects region)))

(def-wl-callback region-subtract (client region (x :int32) (y :int32) (width :int32) (height :int32))
  (push (make-instance 'waylisp:wl-rect :x x :y y :width width :height height :operation :subtract) (rects region)))

(defimplementation wl-region ()
  ((:add region-add)
   (:subtract region-subtract))
  ((rects :accessor rects :initarg :rects :initform nil)))
