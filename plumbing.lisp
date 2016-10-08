
(in-package :ulubis)

(defun initialise-wayland ()
  ;; Define callbacks at run time otherwise save-lisp-and-die executable
  ;; doesn't work
  (defcallback my-commit :void
    ((client-ptr :pointer) (resource :pointer))
    (let* ((client (find-client client-ptr *compositor*))
	   (->surface (wl-resource-get-user-data resource))
	   (surface (find-surface ->surface *compositor*)))
      (setf (committed surface) t)
      (create-texture surface)
      (when (first-commit? surface)
	(first-commit (current-mode) surface))
      (setf (render-needed *compositor*) t)))
  
  (defcallback my-attach :void
    ((client-ptr :pointer) (resource :pointer) (buffer :pointer) (x :int32) (y :int32))
    (let* ((client (find-client client-ptr *compositor*))
	   (->surface (wl-resource-get-user-data resource)))
      (format t "New buffer ~A~%" buffer)
      (setf (->buffer (find-surface ->surface *compositor*)) buffer)))
  
  (defcallback my-frame :void
    ((client-ptr :pointer) (resource :pointer) (callback :uint32))
    (let* ((->surface (wl-resource-get-user-data resource))
	   (client (find-client client-ptr *compositor*))
	   (surface (find-surface ->surface *compositor*)))
      ;;(format t "Client ~A requesting frame callback on surface ~A ~%" client-ptr ->surface)
      ;; gnome-terminal requesting frame callback with no surface
      ;; or has multiple surfaces??
      (when client
	(setf (->frame-callback surface) (wl-resource-create client-ptr wl-callback-interface 1 callback)))))
  
  (defcallback my-set-input-region :void
    ((->client :pointer) (resource :pointer) (region :pointer))
    (let* ((->surface (wl-resource-get-user-data resource))
	   (surface (find-surface ->surface *compositor*)))
      (format t "Setting input region of ~A to  region ~A~%" ->surface region)
      (setf (input-region surface) (find-region-of-client ->client region *compositor*))))
  
  (defcallback set-opaque-region :void
      ((->client :pointer) (resource :pointer) (region :pointer))
    (let* ((->surface (wl-resource-get-user-data resource))
	   (surface (find-surface ->surface *compositor*)))
      (format t "Setting opaque region of ~A to  region ~A~%" ->surface region)
      (setf (opaque-region surface) (find-region-of-client ->client region *compositor*))))
  
  (defparameter surface-implementation
    (implement-wl-surface
     :commit (callback my-commit)
     :attach (callback my-attach)
     :frame (callback my-frame)
     :set-input-region (callback my-set-input-region)
     :set-opaque-region (callback set-opaque-region)))
  
  (defcallback delete-surface :void
    ((resource :pointer))
    (let* ((surface-ptr (wl-resource-get-user-data resource))
	   (surface (find-surface surface-ptr *compositor*)))
      (when (equalp (active-surface *compositor*) surface)
	(setf (active-surface *compositor*) nil))
      (when (equalp (pointer-surface *compositor*) surface)
	(setf (pointer-surface *compositor*) nil))
      (remove-surface surface-ptr *compositor*)
      (setf (render-needed *compositor*) t)
      (format t "Deleting surface ~A~%" surface)))
  
  ;; Compositor
  (defcallback compositor-create-surface :void
    ((client :pointer)
     (resource :pointer)
     (id :uint32)) 
    (let* ((compositor-client (find-client client *compositor*))
	   (new-surface (wl-resource-create client wl-surface-interface 3 id))
	   (ulubis-surface (make-instance 'surface :->surface new-surface :client compositor-client)))
      (push ulubis-surface (surfaces *compositor*))
      (format t "Compositor creating surface ~A for client ~A ~%" new-surface client)
      (wl-resource-set-implementation
       new-surface
       surface-implementation
       new-surface ;;(null-pointer) ;; set to compositor defined surface type
       (callback delete-surface))))
  
  (defcallback region-add :void
    ((client :pointer) (resource :pointer) (x :int32) (y :int32) (width :int32) (height :int32))
    (let ((region (wl-resource-get-user-data resource))
	  (rect (make-instance 'rect :x x :y y :width width :height height :operation :add)))
      ;;      (format t "Region added to ~A (~d,~d,~d,~d)~%" region x y width height)
      (push rect (rects (find-region-of-client client region *compositor*)))))
  
  (defcallback region-subtract :void
    ((client :pointer) (resource :pointer) (x :int32) (y :int32) (width :int32) (height :int32))
    (let ((region (wl-resource-get-user-data resource))
	  (rect (make-instance 'rect :x x :y y :width width :height height :operation :subtract)))
      ;;    (format t "Region subtracted to ~A (~d,~d,~d,~d)~%" region x y width height)
      (push rect (rects (find-region-of-client client region *compositor*)))))
  
  (defparameter region-implementation (implement-wl-region
				       :add (callback region-add)
				       :subtract (callback region-subtract)))
  
  (defcallback compositor-create-region :void
    ((client :pointer) (resource :pointer) (id :uint32))
    ;;(format t "Called compositor create region ~%")
    (let* ((compositor-client (find-client client *compositor*))
	   (new-region (wl-resource-create client wl-region-interface 1 id))
	   (region (make-instance 'region :->region new-region)))
      (setf (regions compositor-client) (push region (regions compositor-client)))
      (wl-resource-set-implementation
       new-region
       region-implementation
       new-region ;; TODO Implement destroy
       (null-pointer))))
  
  (defparameter compositor-implementation (implement-wl-compositor
					   :create-surface (callback compositor-create-surface)
					   :create-region (callback compositor-create-region)))
  
  (defcallback compositor-bind :void
    ((client :pointer) (data :pointer) (version :uint32) (id :uint32))
    (format t "Binding compositor~%")
    (let ((resource (wl-resource-create client wl-compositor-interface 1 id)))
      (wl-resource-set-implementation resource compositor-implementation (null-pointer) (null-pointer))))
  ;; End compositor
  
  (defparameter shell-surface-implementation (implement-wl-shell-surface))
  
  (defcallback shell-get-shell-surface :void
    ((client :pointer) (resource :pointer) (id :uint32) (surface :pointer))
    (format t "wl-shell-get-shell-surface  called")
    (wl-resource-set-implementation
     (wl-resource-create client wl-shell-surface-interface 1 id)
     shell-surface-implementation
     (null-pointer)
     (null-pointer)))
  
  (defparameter shell-implementation (implement-wl-shell
				      :get-shell-surface (callback shell-get-shell-surface)))
  
  ;; Vanilla wl-shell bind
  (defcallback shell-bind :void
    ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
    (format t "~%shell-bind called ~%")
    (let ((new-client (make-instance 'client :->client client-ptr)))
      (push new-client (clients *compositor*))
      (wl-resource-set-implementation
       (wl-resource-create client-ptr wl-shell-interface 1 id)
       shell-implementation
       client-ptr
       (callback client-destroy))))

  (defcallback move :void
      ((client :pointer) (resource :pointer) (seat :pointer) (serial :uint32))
    (format t "Surface requested move~%")
    (let* ((surface-ptr (wl-resource-get-user-data resource))
	   (surface (find-surface surface-ptr *compositor*)))
        (setf (moving-surface *compositor*) surface)))

;;  (defcallback resize :void
  ;;  )    
  
  ;; Re-evaluating this does not update it
  (defcallback print-title :void
      ((client :pointer) (resource :pointer) (title :string))
    ;;(format t "Setting title of ~A to \"~A\" connected~%" client title)
    )
  
  ;; Can redefine this live without redefining xdg-shell-get-xdg-surface
  (defparameter xdg-surface-implementation
    (implement-xdg-surface
     :move (callback move)
;;     :resize (callback resize)
     :set-title (callback print-title)))
  
  (defcallback xdg-shell-get-xdg-surface :void
    ((client-ptr :pointer) (resource :pointer) (id :uint32) (_surface :pointer))
    (format t "Callback to get-xdg-surface~%")
    (let* ((surface-ptr (wl-resource-get-user-data _surface))
	   (xdg-surface (wl-resource-create client-ptr xdg-surface-interface 1 id))
	   (client (find-client client-ptr *compositor*))
	   (surface (find-surface surface-ptr *compositor*)))
      (format t "Surface: ~A~%" surface)
      (setf (->xdg-surface surface) xdg-surface)
      (wl-resource-set-implementation
       xdg-surface
       xdg-surface-implementation
       surface-ptr
       (null-pointer))))

  (defcallback popup-destroy :void
      ((client-ptr :pointer) (resource :pointer))
    (let ((surface-ptr (wl-resource-get-user-data resource)))
      (remove-surface surface-ptr *compositor*)
      (setf (render-needed *compositor*) t)))
  
  (defparameter xdg-popup-implementation
    (implement-xdg-popup
     :destroy (callback popup-destroy)))

  (defcallback xdg-shell-get-xdg-popup :void
      ((client-ptr :pointer) (resource :pointer) (id :uint32) (_surface :pointer) (parent :pointer) (seat :pointer) (serial :uint32) (x :int32) (y :int32))
    (format t "Callback to get-xdg-popup~%")
    (let* ((surface-ptr (wl-resource-get-user-data _surface))
	   (xdg-popup (wl-resource-create client-ptr xdg-popup-interface 1 id))
	   (client (find-client client-ptr *compositor*))
	   (surface (find-surface surface-ptr *compositor*))
	   (parent-surface (find-surface parent *compositor*)))
      (setf (->xdg-popup surface) xdg-popup)
      (setf (x surface) (+ (x parent-surface) x))
      (setf (y surface) (+ (y parent-surface) y))
      (wl-resource-set-implementation
       xdg-popup
       xdg-popup-implementation
       surface-ptr
       (null-pointer))))
      
  (defparameter xdg-shell-implementation
    (implement-xdg-shell
     :get-xdg-surface (callback xdg-shell-get-xdg-surface)
     :get-xdg-popup (callback xdg-shell-get-xdg-popup)))
  
  ;; We delete client here rather than in surface destroy callback
  ;; as a client may connect but not allocate a surface
  (defcallback client-destroy :void
    ((resource :pointer))
    (let* ((->client (wl-resource-get-user-data resource))
	   (client (find-client ->client *compositor*)))
      (format t "Removing client ~A~%" client)
;      (when (equal (active-surface *compositor*) client)
;	(setf (active-client *compositor*) nil))
      (remove-client ->client *compositor*)))
  
  (defcallback xdg-shell-bind :void
    ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
    (format t "xdg-shell-bind called~%")
    (let ((new-client (make-instance 'client :->client client-ptr)))
      (push new-client (clients *compositor*))
      (wl-resource-set-implementation
       (wl-resource-create client-ptr xdg-shell-interface 1 id)
       xdg-shell-implementation
       client-ptr
       (callback client-destroy))))

  (defcallback set-cursor :void
      ((client-ptr :pointer) (resource :pointer) (serial :uint32) (surface-ptr :pointer) (hotspot-x :int32) (hotspot :int32))
    (let ((surface (find-surface surface-ptr *compositor*)))
      (when surface
	(setf (cursor? surface) t)
	(setf (cursor-surface *compositor*) surface))))
  
  ;; Pointer implementation
  (defparameter pointer-implementation (implement-wl-pointer
					:set-cursor (callback set-cursor)))
  
  (defcallback get-pointer :void
    ((client :pointer) (resource :pointer) (id :uint32))
    (let ((pointer (wl-resource-create client wl-pointer-interface 1 id)))
      (wl-resource-set-implementation
       pointer
       pointer-implementation
       (null-pointer)
       (null-pointer))
      (setf (->pointer (find-client client *compositor*)) pointer)))
  
  ;; Keyboard implementation
  
  (defparameter keyboard-implementation (implement-wl-keyboard))
  
  (defcallback get-keyboard :void
    ((client :pointer) (resource :pointer) (id :uint32))
    (let ((keyboard (wl-resource-create client wl-keyboard-interface 1 id)))
      (wl-resource-set-implementation
       keyboard
       keyboard-implementation
       (null-pointer)
       (null-pointer))
      (setf (->keyboard (find-client client *compositor*)) keyboard)
      (multiple-value-bind (fd size) (get-keymap (backend *compositor*))
	(wl-keyboard-send-keymap keyboard 1 fd size))))
  
  (defparameter seat-implementation (implement-wl-seat
					:get-pointer (callback get-pointer)
					:get-keyboard (callback get-keyboard)))
  
  (defcallback seat-bind :void
    ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
    (format t "seat-bind called~%")
    (let ((seat (wl-resource-create client-ptr wl-seat-interface 1 id)))
      (wl-resource-set-implementation
       seat
       seat-implementation
       (null-pointer)
       (null-pointer))
      (wl-seat-send-capabilities seat 3))) ;; WL_SEAT_CAPABILITY_POINTER | WL_SEAT_CAPABILITY_KEYBOARD
  
  )


(defun init-device-manager ()

  (defcallback start-drag :void
      ((client-ptr :pointer) (resource :pointer) (source :pointer) (origin :pointer) (icon :pointer) (serial :uint32))
    (format t "Received start-drag request~%")
    (let* ((seat (wl-resource-get-user-data resource)))
      
    ))
  
  (defcallback set-selection :void
      ((client-ptr :pointer) (resource :pointer) (source :pointer) (serial :uint32))
    (format t "Received set-selection request~%")
    )
  
  (defparameter data-device-implementation
    (implement-wl-data-device
     :start-drag (callback start-drag)
     :set-selection (callback set-selection)))

  (defparameter *data-sources* nil)

  (defcallback offer :void
      ((client-ptr :pointer) (resource :pointer) (mime-type :string))
    (format t "Mime-type offered: ~A~%" mime-type)
    )

  
  (defparameter data-source-implementation
    (implement-wl-data-source
     :offer (callback offer)))
  
  (defcallback create-data-source :void
      ((client-ptr :pointer) (resource :pointer) (id :uint32))
    (format t "Received create-data-source request~%")
    (let ((data-source (wl-resource-create client-ptr wl-data-source-interface
					   (wl-resource-get-version resource)
					   id)))
      (push data-source *data-sources*)
      (wl-resource-set-implementation
       data-source
       data-source-implementation
       data-source
       (null-pointer))
      )
    )
  
  (defcallback get-data-device :void
      ((client-ptr :pointer) (resource :pointer) (id :uint32) (seat :pointer))
    (format t "Received get-data-device request~%")
    (let ((data-device (wl-resource-create client-ptr wl-data-device-interface (wl-resource-get-version resource) id)))
      (push data-device (data-devices *compositor*))
      (wl-resource-set-implementation
       data-device
       data-device-implementation
       seat;;(null-pointer)
       (null-pointer))))
  
  (defparameter data-device-manager-implementation
    (implement-wl-data-device-manager
     :create-data-source (callback create-data-source)
     :get-data-device (callback get-data-device)))
  
  (defcallback device-manager-bind :void
      ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
    (format t "Device manager bind~%")
    (let ((device-manager (wl-resource-create client-ptr wl-data-device-manager-interface 1 id)))
      (wl-resource-set-implementation
       device-manager  
       data-device-manager-implementation
       (null-pointer)
       (null-pointer))))
  
  )

(defun init-wl-output ()

  (defcallback output-bind :void
      ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
    (let ((output (wl-resource-create client-ptr wl-output-interface 1 id)))
      ;;(wl-output-send-scale output 1)
      (setf (->output *compositor*) output)))
  
  (wl-global-create (display *compositor*)
		    wl-output-interface
		    2
		    (null-pointer)
		    (callback output-bind)))

  
