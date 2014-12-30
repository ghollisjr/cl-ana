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

(in-package :cl-ana.linear-algebra)

;;;; Matrices/vectors are simply sequences of sequences/sequences
;;;; since the tensor library provides the more general functionality.

(defun make-matrix (nrows ncols &key (initial-element 0d0))
  (make-tensor (list nrows ncols) :initial-element initial-element))

(defun make-vector (nelts &key (initial-element 0d0))
  (make-tensor (list nelts)
	       :initial-element initial-element))

(defun matrix-transpose (matrix &optional (type 'vector))
  (let* ((matrix-dims (tensor-dimensions matrix))
	 (result (make-tensor (reverse matrix-dims) :type type)))
    (loop
       for indices in (apply #'cartesian-product
			     (loop
				for dim in matrix-dims
				collect (range 0 (1- dim))))
       do (setf (apply #'tensor-ref result (reverse indices))
		(apply #'tensor-ref matrix indices)))
    result))

(defun vector->matrix (vector &key (orientation :column) (type 'vector))
  (let* ((vector-length (first (tensor-dimensions vector))))
    (if (equal orientation :column)
	;; column
	(let ((result (make-tensor (list vector-length 1) :type type)))
	  (loop
	     for i below vector-length
	     do (setf (tensor-ref result i 0)
		      (tensor-ref vector i)))
	  result)
	;; row
	(let ((result (make-tensor (list 1 vector-length) :type type)))
	  (loop
	     for i below vector-length
	     do (setf (tensor-ref result 0 i)
		      (tensor-ref vector i)))
	  result))))

(defun matrix-mult (x y &optional (type 'vector))
  (tensor-contract (list (cons x 1) (cons y 0)) :type type))

(defun euclidean-dot (x y)
  (tensor-contract (list (cons x 0) (cons y 0))))

(defun euclidean-norm2 (x)
  (euclidean-dot x x))

(defun euclidean-norm (x)
  (sqrt (euclidean-norm2 x)))

;; 3-d vector functions:

(defun phi (vector)
  (atan (elt vector 1)
        (elt vector 0)))

(defun theta (vector)
  (acos (/ (elt vector 2)
           (euclidean-norm vector))))

(defun cross-product (a b)
  "Returns cross product of vectors a and b"
  (let ((ax (elt a 0))
        (ay (elt a 1))
        (az (elt a 2))
        (bx (elt b 0))
        (by (elt b 1))
        (bz (elt b 2)))
    (vector (- (* ay bz)
               (* by az))
            (- (- (* ax bz)
                  (* bx az)))
            (- (* ax by)
               (* bx ay)))))
