;;
;; Here we define the concept of a view.
;; Think virtual desktop
;; A view will contain a subset of all surfaces
;; The user should be able to switch between view
;; Each view has its own mode, such that we can have,
;; say, the default mode in one view and some other mode
;; (e.g. a tiling mode, or fixed function mode) in
;; the another view
;;

;; compositor class should hold a number of views
;; Rather than (render (current-mode))
;; we should (render (current-view))
;; which in turn will render the current mode of that view

(in-package :ulubis)

(defclass view ()
  ((modes :accessor modes :initarg :modes :initform nil)
   (default-mode :accessor default-mode :initarg :default-mode :initform nil)
   (surfaces :accessor surfaces :initarg :surfaces :initform nil)
   (active-surface :accessor active-surface :initarg :active-surface :initform nil)
   (effects :accessor effects :initarg :effects :initform nil)
   (fbo :accessor fbo :initarg :fbo :initform nil)
   (fbo-attachment :accessor fbo-attachment :initarg :fbo-attachment :initform nil)
   (fbo-sample :accessor fbo-sample :initarg :fbo-sample :initform nil)))

(defmethod init-view ((view view) &key)
  (let ((fbo (cepl:make-fbo (list 0 :dimensions (list (screen-width *compositor*) (screen-height *compositor*))))))
    (setf (fbo view) fbo)
    (setf (fbo-sample view) (cepl:sample (cepl:attachment-tex fbo 0)))
    (setf (cepl:blending-params fbo) (cepl:make-blending-params))))

;; When a surface is created, it should call add-surface with the
;; current view
;; which can then decide what to do with the surface.
;; On the default mode it would just push the surface onto its

;; Surfaces: surfaces can have their own FBOs (but don't require them) and simply return a texture. Don't think there is any need for them to render themselves
;; Modes: modes could have their own FBOs...but I don't think this is necessary...only one mode should be in operation on any particular view
;;        therefore we have two options: 1) the containing view passes in its own fbo, and the mode #'map-g-into's this fbo, 2) otherwise it would
;;        require its own fbo, #'map-g-into and then pass back a texture for the containing view to use. The latter doesn't seem correct so we
;;        we go for 1. I suppose the mode could also have EFFECTS, defining other FBOs that can first be rendered into before rendering into the view's FBO.
;; View: each view has a least one FBO? The view asks its current mode to render into its FBO. The view can have EFFECTS, which have their own FBOs which can be rendered into first before finally rendering into the views FBO. (render view) would return a texture sampler of its FBO (or should it be (texture-of view))? 
;; Screen: The screen doesn't neep an FBO (it can just #'map-g with the texture of the current view, or ask for the textures of a number of views and composite them somehow). The screen can also have EFFECTS.

(defmethod add-surface ((view view) surface)
  (push surface (surfaces view)))

(defun current-mode (view)
  (with-slots (modes default-mode) view
    (let ((mode (first modes)))
      (if mode
	  mode
	  default-mode))))

#|
(defun current-view (
  (if (modes view)
      (first (modes view))
      (default-mode view)))
|#

(defun push-view (default-mode)
  (let ((view (make-instance 'view :default-mode (make-instance default-mode))))
    (setf (view (default-mode view)) view)
    (init-mode (default-mode view))
    (init-view view)
    (push view (views *compositor*))))

(defmethod texture-of ((view view))
  (let ((current-mode (current-mode view)))
    (with-slots (fbo fbo-sample) view
      (with-screen (vertex-stream)
	(cepl:clear fbo)
	(render current-mode fbo)
	(cepl:map-g-into fbo #'passthrough-shader vertex-stream :texture fbo-sample)
	fbo-sample))))
    
