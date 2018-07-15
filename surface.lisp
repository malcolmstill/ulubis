
(in-package :ulubis)

(defmethod keyboard-handler ((surface isurface) time keycode keysym state)
  (when (and keycode (keyboard (client surface)))
    (wl-keyboard-send-key (->resource (keyboard (client surface))) 0 time keycode state))
  (when (and (keyboard (client surface)))
    (wl-keyboard-send-modifiers (->resource (keyboard (client surface))) 0
				(mods-depressed *compositor*)
				(mods-latched *compositor*)
				(mods-locked *compositor*)
				(mods-group *compositor*))))


(defmethod cancel-mods ((surface isurface))
  (when (and (keyboard (client surface)))
    (wl-keyboard-send-modifiers (->resource (keyboard (client surface))) 0
				0
				0
				0
				0)))

(defun effective-width (surface)
  (if (input-region (wl-surface surface))
      (width (first (last (rects (input-region (wl-surface surface))))))
      (width (wl-surface surface))))

(defun effective-height (surface)
  (if (input-region (wl-surface surface))
      (height (first (last (rects (input-region (wl-surface surface))))))
      (height (wl-surface surface))))
