(require 'generic-math)

(require 'histogram)

(require 'error-propogation)

(in-package :histogram)

(defparameter *ndata* 1000000)
(defparameter *x-data* nil)
(defparameter *y-data* nil)

(defun fill-random ()
  (setf *x-data* nil)
  (setf *y-data* nil)
  (iter
    (for i from 0 to (1- *ndata*))
    (push (* 2d0 (/ (random 100) 100)) *x-data*)
    (push (* 2d0 (/ (random 100) 100)) *y-data*)))

(defun contiguous-hist-test ()
  (defparameter *hist*
    (make-contiguous-hist (list (list :name "x"
				      :nbins 10
				      :low 0d0
				      :high 5d0)
				(list :name "y"
				      :nbins 3
				      :low 0
				      :high 2))
			  :default-increment #e(1 1)))

  ;;(fill-random)

  (defparameter *data*
    (mapcar #'list *x-data* *y-data*))
  
  (dolist (d *data*)
    (hist-insert *hist* d))
  
  (hist-integrate *hist* "x"))

(defun sparse-hist-test ()
  (defparameter *hist*
    (make-sparse-hist (list (list :name "x"
				      :nbins 10
				      :low 0d0
				      :high 5d0)
				(list :name "y"
				      :nbins 3
				      :low 0
				      :high 2))
			  :default-increment #e(1 1)))
  
  (defparameter *ndata* 1000000)

  ;;(fill-random)

  (defparameter *data*
    (mapcar #'list *x-data* *y-data*))
  
  (dolist (d *data*)
    (hist-insert *hist* d))
  
  (hist-integrate *hist* "x"))
