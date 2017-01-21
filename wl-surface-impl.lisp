
(in-package :ulubis)

(defimplementation wl-callback ()
  ()
  ())

(def-wl-callback commit (client surface)
  (setf (committed surface) t)
  (create-texture surface)
  (when (and (buffer surface) (first-commit? surface))
    (first-commit (current-mode (current-view *compositor*)) (role surface)))
  (setf (render-needed *compositor*) t))

(def-wl-callback attach (client surface (buffer :pointer) (x :int32) (y :int32))
  (setf (buffer surface) buffer))
  
(def-wl-callback frame (client surface (callbackid :uint32))
  (let ((frame-callback (make-wl-callback (->client client) 1 callbackid :implementation? nil)))
    (setf (frame-callback surface) frame-callback)
    (push frame-callback (callbacks *compositor*))))

(def-wl-callback set-input-region (client surface (region :pointer))
  (setf (input-region surface) (find-resource client region)))


(def-wl-callback set-opaque-region (client surface (region :pointer))
  (setf (opaque-region surface) (find-resource client region)))

(def-wl-callback surface-destroy (client surface)
  (setf (role surface) nil)
  (setf (wl-surface surface) nil)
  (setf (callbacks *compositor*) (remove (frame-callback surface) (callbacks *compositor*)))
  (setf (frame-callback surface) nil))

(defimplementation wl-surface (isurface)
  ((:commit commit)
   (:attach attach)
   (:frame frame)
   (:set-input-region set-input-region)
   (:set-opaque-region set-opaque-region)
   (:destroy surface-destroy))
  ((frame-callback :accessor frame-callback :initarg :frame-callback :initform nil)
   (committed :accessor committed :initarg :committed :initform nil)
   (input-region :accessor input-region :initarg :input-region :initform nil)
   (opaque-region :accessor opaque-region :initarg :opaque-region :initform nil)
   (texture :accessor texture :initarg :texture :initform nil)
   (role :accessor role :initarg :role :initform nil)
   (buffer :accessor buffer :initarg :buffer :initform nil)
   (first-commit? :accessor first-commit? :initarg :first-commit? :initform t)))
