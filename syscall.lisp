
(defpackage :syscall
  (:use :common-lisp :cffi)
  (:export
   poll
   poll-return-event
   with-pollfds
   pollin
   pollpri
   pollout
   pollerr
   pollhup
   pollnval
   ioctl))

(in-package :syscall)

(defconstant pollin 1)
(defconstant pollpri 2)
(defconstant pollout 4)
(defconstant pollerr 8)
(defconstant pollhup 16)
(defconstant pollnval 32)

(defcstruct pollfd
  (fd :int)
  (events :short)
  (revents :short))

(defcfun ("poll" %poll) :int
  (fds     :pointer)
  (nfds    :unsigned-long)
  (timeout :int))

(defun poll (pollfds nfds timeout)
  "Wait for events on file descriptors defined by POLLFDS. TIMEOUT is the time in milliseconds to wait for activity; a TIMEOUT of -1 will block indefinitely, a TIMEOUT of 0 will return immediately"
  (let ((r (%poll pollfds nfds timeout)))
    (when (= r -1)
      (nix:posix-error))
    (> r 0)))
    
(defun poll-return-event (pollfd)
  "Access REVENT of pollfd struct"
  (with-foreign-slots ((revents) pollfd (:struct pollfd))
    revents))

(defmacro with-pollfds ((name &rest specs) &body body)
  (let ((nfds (length specs)))
    `(with-foreign-object (,name '(:struct pollfd) ,nfds)
       (let (,@(loop :for i :from 0 :to (- nfds 1)
                  :collecting (let ((spec (nth i specs)))
                        `(,(first spec) (mem-aptr ,name '(:struct pollfd) ,i)))))
         ,@(loop :for i :from 0 :to (- nfds 1)
              :collecting (let ((spec (nth i specs)))
                    `(with-foreign-slots ((fd events revents) ,(first spec) (:struct pollfd))
                       (setf fd ,(second spec))
                       (setf events (logior ,@(rest (rest spec))))
                       (setf revents 0))))
         ,@body))))

(defcfun ("ioctl" %ioctl-with-integer-arg) :int
 (fd      :int)
 (request :int)
 (arg     :unsigned-long))

(defun ioctl (fd request &optional (arg nil argp))
  (cond
    ((not argp) (nix:ioctl fd request))
    ((pointerp arg) (nix:ioctl fd request))
    ((integerp arg) (%ioctl-with-integer-arg fd request arg))))
