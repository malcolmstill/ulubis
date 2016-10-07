
(defmacro defmenu (name () &body body)
  )

(defmenu default-menu ()
  (text "Menu")
  (launch "weston-terminal" "Terminal")
  (text "End menu"))


(defun create-menu (entries)
  (let* ((surface (cairo:create-image-surface :argb32 1 1))
	 (cr (cairo:create-context surface)))
    (loop :for e :in entries
       :do (progn 
  
