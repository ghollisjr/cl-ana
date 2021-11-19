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

;; Solving linear equations via Gauss-Jordan Elimination.  Of course
;; this is not the most efficient algorithm, but all the generally
;; available linear algebra libraries are focused on floating point
;; arithmetic.  This simple implementation of Gaussian Elimination
;; will represent data as integers, rational values, or floating point
;; values depending on the input given, which is useful for
;; academic/educatonal examples.
(defun linsolve (A B)
  "Solves the linear equation A x = B for x using Gaussian
Elimination.  A should be a 2-D tensor containing the coefficients,
and B should be a 2-D column tensor or a 1-D tensor of values for the
right-hand-side.

Returns a list of the solution values x if solvable, or nil if no
solution is possible, e.g. singular matrix."
  (when (and A B)
    (let* ((coefs (map 'vector
                       (lambda (x)
                         (coerce x 'vector))
                       A))
           (rhs (coerce (if (= (tensor-rank B) 1)
                            (loop
                               for i below (length B)
                               collecting (tensor-ref B i))
                            (loop
                               for i below (length B)
                               collecting (tensor-ref B i 0)))
                        'vector))
           (dims (tensor-dimensions A))
           (nrows (first dims))
           (ncols (second dims)))
      (labels ((leading (col)
                 ;; returns index to first row with non-zero entry at col dimension
                 (loop
                    for i upfrom col below nrows
                    when (not (zerop (tensor-ref coefs i col)))
                    do (return i)
                    finally (return nil)))
               (normalize-row (row col)
                 ;; normalizes row using value at col
                 (let* ((val (tensor-ref coefs row col)))
                   (loop
                      for i below ncols
                      do (setf (tensor-ref coefs row i)
                               (/ (tensor-ref coefs row i)
                                  val)))
                   (setf (aref rhs row)
                         (/ (aref rhs row)
                            val))))
               (eliminate-row (row col basis)
                 ;; eliminates row using the (normalized) basis row for col
                 (let* ((ratio (tensor-ref coefs row col)))
                   (loop
                      for j upfrom col below ncols
                      do (setf (tensor-ref coefs row j)
                               (- (tensor-ref coefs row j)
                                  (* ratio (tensor-ref coefs basis j)))))
                   (setf (aref rhs row)
                         (- (aref rhs row)
                            (* ratio (aref rhs basis))))))
               (swap-rows (r1 r2)
                 ;; swaps rows r1 and r2
                 (when (not (= r1 r2))
                   (loop
                      for j below ncols
                      for tmp = (tensor-ref coefs r1 j)
                      do
                        (setf (tensor-ref coefs r1 j)
                              (tensor-ref coefs r2 j))
                        (setf (tensor-ref coefs r2 j)
                              tmp))
                   (let* ((tmp (aref rhs r1)))
                     (setf (aref rhs r1)
                           (aref rhs r2))
                     (setf (aref rhs r2)
                           tmp)))))
        (loop
           for j below ncols
           for leading = (leading j)
           do
             (when (null leading) ; singular matrix
               (return-from linsolve nil))
             (normalize-row leading j)
             (loop
                for i below nrows
                when (not (= i leading))
                do
                  (eliminate-row i j leading))
             (swap-rows j leading))
        (coerce rhs 'list)))))

(defun lisp-2d-array->tensor (arr)
  (let* ((dims (array-dimensions arr))
         (n (product dims))
         (result (make-tensor dims)))
    (loop
       for i below n
       do (setf (tensor-flat-ref result i)
                (apply #'aref arr (cl-ana.tensor::unflatten-index i dims))))
    (map 'list
         (lambda (x)
           (coerce x 'list))
         result)))

(defun grid->lisp-array (grid)
  (cffi:convert-from-foreign
   (grid::foreign-pointer grid)
   (list* :array
          ;; (grid::element-type solution)
          (let* ((type
                  (grid:cl-cffi
                   (grid::element-type grid))))
            type)
          (grid::dimensions grid))))

;; Frontend to GSLL's lu-solve:
(defun lu-solve (A B)
  "Frontend to GSLL.  Solves linear equation A x = B.  A should be a
list of lists, and B should be a list."
  (let* ((matA
          (grid:make-foreign-array
           'double-float
           :initial-contents (->double-float A)))
         (matB
          (grid:make-foreign-array
           'double-float
           :initial-contents
           (->double-float
            B))))
    (multiple-value-bind (matrix perm)
        (gsll:lu-decomposition matA)
      (let* ((solution
              (gsll:lu-solve matrix matB perm))
             (converted
              (grid->lisp-array solution)))
        (coerce converted
                'list)))))

;; Frontend to GSLL's lu-invert:
(defun lu-invert (matrix)
  "Frontend to GSLL.  Inverts matrix.  matrix should be a list of
lists."
  (let* ((mat
          (grid:make-foreign-array
           'double-float
           :initial-contents (->double-float matrix))))
    (multiple-value-bind (matrix perm)
        (gsll:lu-decomposition mat)
      (let* ((result
              (gsll:lu-invert matrix perm)))
        (map 'list
             (lambda (x)
               (coerce x 'list))
             (lisp-2d-array->tensor
              (grid->lisp-array result)))))))

;; Frontend to GSLL's LU-determinant
(defun lu-determinant (matrix)
  "Frontend to GSLL.  matrix should be a list of lists."
  (let* ((mat
          (grid:make-foreign-array
           'double-float
           :initial-contents (->double-float matrix))))
    (multiple-value-bind (matrix perm signum)
        (gsll:lu-decomposition mat)
      (gsll:lu-determinant matrix signum))))

;; Eigendecomposition:
(defun eigen-decomposition (matrix)
  "Returns eigen values and eigen vectors of the matrix.  Frontend to
GSLL.

Returns:

* eigenvalue list
* eigenvector list"
  (let* ((mat (grid:make-foreign-array 'double-float
                                       :dimensions (tensor-dimensions matrix)
                                       :initial-contents matrix)))
    (multiple-value-bind (values vectors)
        (gsll:eigenvalues-eigenvectors-nonsymm mat)
      (values (coerce (grid->lisp-array values) 'list)
              (transpose
               (map 'list
                    (lambda (x)
                      (coerce x 'list))
                    (lisp-2d-array->tensor
                     (grid->lisp-array vectors))))))))
