
(defpackage :easing
  (:use :common-lisp)
  (:export
   duration
   start-time
   start-value
   end-value
   easing
   linear
   value-at-time))

(in-package :easing)

(defclass easing ()
  ((duration :accessor duration :initarg :duration :initform 1000)
   (start-time :accessor start-time :initarg :start-time :initform 0)
   (start-value :accessor start-value :initarg :start-value :initform 0.0)
   (end-value :accessor end-value :initarg :end-value :initform 1.0)))

(defgeneric value-at-time (easing time))

(defclass linear (easing) ())

(defmethod value-at-time ((easing linear) time)
  (with-slots (start-time start-value end-value duration) easing
    (coerce
     (if (>= (- time start-time) duration)
	 end-value
	 (+ (* (/ (- end-value start-value)
		  duration)
	       (- time start-time))
	    start-value))
     'float)))


