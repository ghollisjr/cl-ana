;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013, 2014 Gary Hollis
;;;; 
;;;; This file is part of cl-ana.
;;;; 
;;;; cl-ana is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; cl-ana is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with cl-ana.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis (me!) via email at
;;;; ghollisjr@gmail.com

(require 'cl-ana)

(in-package :cl-ana)

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
			  :default-increment (+- 1 1)))

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
                      :default-increment (+- 1 1)))
  
  (defparameter *ndata* 1000000)
  
  ;;(fill-random)
  
  (defparameter *data*
    (mapcar #'list *x-data* *y-data*))
  
  (dolist (d *data*)
    (hist-insert *hist* d))
  
  (hist-integrate *hist* "x"))
