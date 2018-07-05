
(in-package :ulubis)

(defun screenshot (&optional (filename "screenshot.png"))
  (with-slots ((width screen-width) (height screen-height)) *compositor*
    (let* ((png (make-instance 'zpng:pixel-streamed-png
			       :color-type :truecolor-alpha
			       :width width
			       :height height))
	   (bytes 3)
	   (pixels (gl:read-pixels 0 0 width height :bgr :unsigned-byte)))
      (with-open-file (stream filename
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create
			      :element-type '(unsigned-byte 8))
	(zpng:start-png png stream)
	(loop :for y :from 0 :to (- height 1) :do
	     (loop :for x :from 0 :to (- width 1) :do
		  (let ((i (* x bytes))
			(j (- (* (- height y) width bytes) (* width bytes))))
		    (zpng:write-pixel
		     (list (aref pixels (+ i j 0))
			   (aref pixels (+ i j 1))
			   (aref pixels (+ i j 2))
			   255)
		     png))))
	(zpng:finish-png png)))))
