;;;; lorentz.lisp

(in-package :lorentz)

;;; This library of lorentz transformation-related utilities assumes
;;; dogmatically that
;;;
;;; 1. Time component is the zeroth element.
;;; 2. We are using the (+,-,-,...) Minkowski space metric.
;;;
;;; This implementation does lack a general lorentz transformation as
;;; it only provides the boost facilities.  But: you can do any
;;; general lorentz transformation by using a boost followed by a
;;; rotation.
;;;
;;; The library also provides the useful reader macro for lorentz
;;; vectors: #L(...), which accepts exactly 4 values inside of the
;;; parentheses which will become the time, x, y, and z components
;;; respectively.

(defclass lorentz-vector (vector-like)
  ((val-vector
   :initarg :val-vector
   :initform (make-double-float-vector (list 0 0 0 0))
   :accessor lorentz-vector-val-vector)))

(defmethod print-object ((l lorentz-vector) stream)
  (format stream "#L(")
  (iter
    (for vector-index from 0 below 3)
    (format stream "~a " (vref l vector-index)))
  (format stream "~a :~a)" (vref l 3)
	  (lisp-matrix::vector-orientation
	   (lorentz-vector-val-vector l))))

(defun make-lorentz-vector (&optional
			      (time 0d0)
			      (x 0d0)
			      (y 0d0)
			      (z 0d0)
			      (type :column))
  (let ((val-vector
	 (make-double-float-vector (list time x y z)
				   type)))
    (make-instance 'lorentz-vector :val-vector val-vector)))

(defmethod transpose-matrix ((l lorentz-vector))
  (make-instance 'lorentz-vector
		 :val-vector
		 (transpose-matrix
		  (lorentz-vector-val-vector l))))

(defmethod vref ((l lorentz-vector) index)
  (vref (lorentz-vector-val-vector l) index))

(defmethod add ((v1 lorentz-vector) (v2 lorentz-vector))
  (v+ (lorentz-vector-val-vector v1)
      (lorentz-vector-val-vector v2)))

(defmethod sub ((v1 lorentz-vector) (v2 lorentz-vector))
  (make-instance 'lorentz-vector
		 :val-vector
		 (v- (lorentz-vector-val-vector v1)
		     (lorentz-vector-val-vector v2))))

(defmethod mult ((v1 lorentz-vector) (v2 lorentz-vector))
  (minkowski-dot v1
		 v2))

(defmethod-commutative mult (x (l lorentz-vector))
  (make-instance 'lorentz-vector
		 :val-vector
		 (scal (float x 0d0)
		       (lorentz-vector-val-vector l))))

(defmethod mult ((l lorentz-vector) (m matrix-like))
  (let* ((val-vector (lorentz-vector-val-vector l))
	 (result-val-vector (m* val-vector m)))
    (make-instance 'lorentz-vector
		   :val-vector result-val-vector)))

(defmethod mult ((m matrix-like) (l lorentz-vector))
  (let* ((val-vector (lorentz-vector-val-vector l))
	 (result-val-vector (m* m val-vector)))
    (make-instance 'lorentz-vector
		   :val-vector result-val-vector)))

(defmethod div ((l lorentz-vector) x)
  (make-instance 'lorentz-vector
		 :val-vector
		 (scal (unary-div (float x 0d0))
		       (lorentz-vector-val-vector l))))

;; Reader macro:
(defun lorentz-vector-transformer-reader-macro (stream subchar arg)
  (let ((expr (read stream t)))
    `(make-lorentz-vector ,@expr)))

(set-dispatch-macro-character
 #\# #\l #'lorentz-vector-transformer-reader-macro)

(defun minkowski-dot (left-vector right-vector)
  "Computes the inner product using the Minkowski metric; only
requires the vref function to be defined and that the vectors each
have exactly 4 elements each."
  (flet ((variance-flip-factor (i)
	   (if (zerop i)
	       1
	       -1)))
     (with-accessors ((left-length lisp-matrix::vector-dimension))
	 (lorentz-vector-val-vector left-vector)
       (with-accessors ((right-length lisp-matrix::vector-dimension))
	   (lorentz-vector-val-vector right-vector)
	 (loop
	    for i from 0 below left-length
	    for j from 0 below right-length
	    summing (* (variance-flip-factor i)
		       (vref left-vector i)
		       (vref right-vector j)))))))

(defun minkowski-norm (vector)
  (sqrt (minkowski-norm2 vector)))

(defun minkowski-norm2 (vector)
  (minkowski-dot vector vector))

(defun make-lorentz-boost (beta-vector)
  "Construct lorentz boost matrix from beta vector"
  (let* ((lorentz-boost (make-matrix 4 4))
	 (beta2 (euclidean-norm2 beta-vector))
	 (gamma (gamma-from-beta2 beta2)))
    ;; matrix is symmetric, so we can fill one side and then set the other to be the same
    ;; main diagonal:
    ;; top-left corner:
    (setf (mref lorentz-boost 0 0)
	  gamma)
    ;; spatial diagonal:
    (loop
       for i from 1 to 3
       do (setf (mref lorentz-boost i i)
		(+ 1
		   (* (- gamma 1)
		      (/ (expt (vref beta-vector (1- i)) 2)
			 beta2)))))
    ;; spatial off-diagonal:
    (loop
       for i from 1 to 2
       do (loop
	     for j from (1+ i) to 3
	     do (progn
		  (setf (mref lorentz-boost i j)
			(* (- gamma 1)
			   (vref beta-vector (1- i))
			   (vref beta-vector (1- j))
			   (/ beta2)))
		  (setf (mref lorentz-boost j i)
			(mref lorentz-boost i j)))))    
    ;; rest of first row & first column
    (loop
	 for i from 1 to 3
	 do (progn
	      (setf (mref lorentz-boost 0 i)
		    (- (* gamma
			  (vref beta-vector (1- i)))))
	      (setf (mref lorentz-boost i 0)
		    (mref lorentz-boost 0 i))))
    lorentz-boost))

;;; miscellaneous physics functions:

(defun gamma (beta)
  "Returns gamma factor from beta"
  (gamma-from-beta2 (expt beta 2)))

(defun gamma-from-beta2 (beta2)
  "Computes gamma from beta^2, for efficiency purposes"
  (/ (sqrt (- 1
	      beta2))))

(defun lorentz-vector-spatial (vector)
  "Returns spatial part of the lorentz-vector"
  (with-accessors ((length lisp-matrix::vector-dimension)
		   (orientation lisp-matrix::vector-orientation))
      (lorentz-vector-val-vector vector)
    (let ((spatial-part (make-vector (1- length) :type orientation)))
      (loop
	 for i from 1 below length
	 do (setf (vref spatial-part (1- i))
		  (vref vector i)))
      spatial-part)))

(defun four-momentum-beta-vector (four-momentum)
  "Returns the beta vector from the four-momentum.  Assumes that your
units are chosen such that c is effectively 1 (e.g. GeV/c^2 for mass,
GeV/c for momentum, etc.)"
  (let ((momentum-vector (lorentz-vector-spatial four-momentum))
	(energy (vref four-momentum 0)))
    (scal (/ energy)
	  momentum-vector)))
