
(in-package :ulubis)

(def-wl-callback get-toplevel (client zxdg-surface (id :uint32))
  (let ((toplevel (make-zxdg-toplevel-v6 client 1 id :delete-fn (callback zxdg-toplevel-delete))))
    ;; Save the xdg-surface object so that configure events can be sent
    (setf (zxdg-surface-v6 toplevel) zxdg-surface)
    ;; Surface role now becomes xdg-toplevel
    (setf (role (wl-surface zxdg-surface)) toplevel)
    ;; Save the wl-surface associated with the toplevel
    (setf (wl-surface toplevel) (wl-surface zxdg-surface))
    (push toplevel (surfaces (current-view *compositor*)))
    #|
    (with-wl-array array
      (zxdg-toplevel-v6-send-configure (->resource toplevel) 0 0 array)
      (zxdg-surface-v6-send-configure (->resource zxdg-surface) 0))))
    |#
    (let ((array (foreign-alloc '(:struct wl_array))))
      (wl-array-init array)
      (zxdg-toplevel-v6-send-configure (->resource toplevel) 0 0 array)
      (zxdg-surface-v6-send-configure (->resource zxdg-surface) 0))))

(defimplementation zxdg-surface-v6 (isurface)
  ((:get-toplevel get-toplevel))
  ())
