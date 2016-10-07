
(defpackage :animation
  (:use :common-lisp :easing)
  (:export
   animation
   sequential-animation
   parallel-animation
   update-animations
   remove-animation
   start-animation
   target
   property
   to
   from
   duration
   start-time
   easing-fn
   integer?))

(in-package :animation)

(defparameter *animations* nil)

(defclass animation ()
  ((target :accessor target :initarg :target :initform nil)
   (property :accessor property :initarg :property :initform nil)
   (from :accessor from :initarg :from :initform nil)
   (to :accessor to :initarg :to :initform nil)
   (easing-fn :accessor easing-fn :initarg :easing-fn :initform nil)
   (start-time :accessor start-time :initarg :start-time :initform 0)
   (duration :accessor duration :initarg :duration :initform 0)
   (toplevel? :accessor toplevel? :initarg :toplevel? :initform t)
   (integer? :accessor integer? :initarg :integer? :initform nil)
   (finished-fn :accessor finished-fn :initarg :finished-fn :initform nil)))

(defun normalise-time (time start-time duration)
  (/ (- time start-time) duration))

(defun normalise-output (from to value)
  (coerce (+ (* value (- to from)) from) 'float))

#|
(defmethod initialize-instance :after ((animation animation) &key)
  (with-slots (target property from to) animation
    (when (and target property (not from))
      (setf (slot-value animation 'from) (slot-value target property)))
    (when (and target property (not to))
      (setf (slot-value animation 'to) (slot-value target property)))))
|#

;; Improvement: maybe we should be able to pass &rest rest parameters to finished-fn
(defmethod start-animation ((animation animation) &key (time (get-internal-real-time)) finished-fn (toplevel t))
  (setf (start-time animation) time)
  (setf (toplevel? animation) toplevel)
  (setf (finished-fn animation) finished-fn)
  (when (not (from animation))
    (setf (from animation) (slot-value (target animation) (property animation))))
  (when toplevel
    (push animation *animations*)))

#|
Let's have update animation return true if it is still running,
otherwise false. This will allow container animations to move to other animations
etc.
|#
(defmethod update-animation ((animation animation) time)
  (with-slots (target property to from easing-fn duration start-time finished-fn toplevel?) animation
    (cond
      ((< time start-time) t)
      ((and (>= time start-time) (<= time (+ start-time duration)))
       (let ((result (normalise-output from
				       to
				       (funcall easing-fn
						(normalise-time time
								start-time
								duration)))))
	 (setf (slot-value target property) (if (integer? animation)
						(round result)
						result))
	 t))
      (t (progn
	   (setf (slot-value target property) to)
	   (when finished-fn
	     (funcall finished-fn))
	   (when toplevel?
	     (remove-animation animation)))))))

(defclass parallel-animation (animation)
  ((animations :accessor animations :initarg :animations :initform nil)))

(defmethod initialize-instance :after ((animation parallel-animation) &key)
  (when (or (from animation) (to animation))
    (error "Parallel animations do not take from or to arguments")))

(defmethod start-animation ((animation parallel-animation) &key (time (get-internal-real-time)) finished-fn (toplevel t))
  (setf (toplevel? animation) toplevel)
  (mapcar (lambda (a)
	    (start-animation a :time time :toplevel nil))
	  (animations animation))
  (when toplevel
    (push animation *animations*)))

(defmethod update-animation ((animation parallel-animation) time)
  (let ((statuses (mapcar (lambda (a)
			    (update-animation a time))
			  (animations animation))))
    (if (reduce (lambda (a b) (or a b)) statuses) ;; if any animations are still running...
	t ;; return true
	(progn
	  (when (finished-fn animation) ;; otherwise all are finished 
	    (funcall (finished-fn animation)))
	  (when (toplevel? animation)
	    (remove-animation animation))
	  nil))))

(defclass sequential-animation (animation)
  ((animations :accessor animations :initarg :animations :initform nil)
   (remaining :accessor remaining :initarg :remaining :initform nil)))

(defmethod initialize-instance :after ((animation sequential-animation) &key)
  (when (or (from animation) (to animation))
    (error "Sequential animations do not take from or to arguments")))

(defmethod start-animation ((animation sequential-animation) &key (time (get-internal-real-time)) finished-fn (toplevel t))
  (setf (remaining animation) (animations animation))
  (start-animation (first (animations animation)) :time time :toplevel nil)
  (setf (toplevel? animation) toplevel)
  (when toplevel
    (push animation *animations*)))

(defmethod update-animation ((animation sequential-animation) time)
  (with-slots (animations finished-fn toplevel? remaining) animation
    (let ((status (update-animation (first remaining) time)))
      (if (not status)
	  (progn
	    ;; Previous animation has finished
	    (setf remaining (rest remaining))
	    (if remaining
		(progn
		  (format t "Starting animation  ~A~%" (first remaining))
		  (start-animation (first remaining) :time time :toplevel nil) ;; More animations to run
		  t)
		(progn
		  (when finished-fn ;; No more animations to run
		    (funcall finished-fn))
		  (when toplevel?
		    (remove-animation animation)))))
	  t))))

(defun remove-animation (animation)
  (setf *animations* (remove animation *animations*)))

(defun update-animations (callback)
  (when *animations*
    (funcall callback))
  (let ((time (get-internal-real-time)))
    (loop :for a :in *animations*
       :do (update-animation a time))))
