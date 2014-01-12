;;;; linear-algebra.lisp

(in-package :linear-algebra)

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
  (tensor-contract type (list x 1) (list y 0)))

(defun euclidean-dot (x y)
  (tensor-contract 'vector (list x 0) (list y 0)))

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
