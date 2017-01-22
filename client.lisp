
(in-package :ulubis)

#|
(defclass ulubis-surface (waylisp:wl-surface)
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)
   (origin-x :accessor origin-x :initarg :origin-x :initform 0.0)
   (origin-y :accessor origin-y :initarg :origin-y :initform 0.0)
   (scale-x :accessor scale-x :initarg :scale-x :initform 1.0)
   (scale-y :accessor scale-y :initarg :scale-y :initform 1.0)
   (opacity :accessor opacity :initarg :opacity :initform 1.0)
   (effects :accessor effects :initarg :effects :initform nil)
   (committed :accessor committed :initarg :committed :initform nil)
   (first-commit? :accessor first-commit? :initarg :first-commit? :initform t)))

;; XDG

(defclass ulubis-xdg-surface (ulubis-surface waylisp:xdg-surface)
  ())

(defun ulubis-xdg-surface? (surface)
  (eql (class-of surface) (find-class 'ulubis-xdg-surface)))

(defclass ulubis-xdg-popup (ulubis-surface waylisp:xdg-popup)
  ())

;; ZXDG

(defclass ulubis-zxdg-surface (ulubis-surface waylisp:zxdg-surface)
  ())

(defun ulubis-zxdg-surface? (surface)
  (eql (class-of surface) (find-class 'ulubis-zxdg-surface)))

(defclass ulubis-zxdg-toplevel (ulubis-zxdg-surface waylisp:zxdg-toplevel)
  ())

(defun ulubis-zxdg-toplevel? (surface)
  (eql (class-of surface) (find-class 'ulubis-zxdg-toplevel)))

(defclass ulubis-zxdg-popup (ulubis-zxdg-surface waylisp:zxdg-popup)
  ())

(defclass ulubis-subsurface (ulubis-surface waylisp:wl-subsurface)
  ())

(defclass ulubis-cursor (ulubis-surface waylisp:wl-cursor)
  ())

(defun ulubis-cursor? (surface)
  (eql (class-of surface) (find-class 'ulubis-cursor)))

|#
