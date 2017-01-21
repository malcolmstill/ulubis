
(in-package :ulubis)

(def-wl-callback get-xdg-surface (client zxdg-shell (id :uint32) (surface-ptr :pointer))
  (let* ((surface (find-resource client surface-ptr))
	 (zxdg-surface (make-zxdg-surface-v6 client 1 id)))
    (setf (wl-surface zxdg-surface) surface)
    (setf (role surface) zxdg-surface)))

(defimplementation zxdg-shell-v6 ()
  ((:get-xdg-surface get-xdg-surface))
  ())

#|
(defcallback client-delete :void ((client-ptr :pointer))
	     (remove-client (get-client client-ptr)))
|#
(def-wl-delete client-delete (zxdg-shell)
  (when zxdg-shell
    (format t "DELETING CLIENT~%")
    (remove-client (->client (client zxdg-shell)))
    (setf (render-needed *compositor*) t))
  )

(def-wl-bind zxdg-shell-v6-bind (client (data :pointer) (version :uint32) (id :uint32))
  (let ((shell (make-zxdg-shell-v6 client 1 id :delete-fn (callback client-delete))))
    (format t "Made zxdg-shell: ~A~%" shell)))

#|
(defcallback zxdg-shell-v6-bind :void ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
  (bind-zxdg-shell-v6 client-ptr 1 id :delete-fn (callback client-delete)))
|#

#|
(defcallback zxdg-shell-v6-bind :void
    ((client-ptr :pointer) (data :pointer) (version :uint32) (id :uint32))
  (format t "zxdg-shell-bind called: ~A~%" client-ptr)
  ;;(waylisp:get-client client-ptr)
  (wl-resource-set-implementation
   (wl-resource-create client-ptr zxdg-shell-v6-interface 1 id)
   zxdg-shell-v6-implementation
   client-ptr
   (callback client-delete)))
|#
