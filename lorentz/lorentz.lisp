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

(defclass lorentz-vector ()
  ((val-vector
    :accessor lorentz-val-vector
    :initform (make-vector 4 :type :column)
    :initarg :val-vector
    :documentation "lorentz vector contents")))

(defmethod print-object ((l lorentz-vector) stream)
  (with-accessors ((val-vector lorentz-val-vector))
      l
    (format stream "#L(")
    (iter
      (for vector-index from 0 below 3)
      (format stream "~a " (vref val-vector vector-index)))
    (format stream "~a)" (vref val-vector 3))))

(defun make-lorentz-vector (time x y z)
  (make-instance 'lorentz-vector
		 :val-vector (make-double-float-vector
			      (list time x y z) :column)))
    

(defmethod add ((v1 lorentz-vector) (v2 lorentz-vector))
  (make-instance 'lorentz-vector
		 :val-vector
		 (v+ (lorentz-val-vector v1)
		     (lorentz-val-vector v2))))

(defmethod sub ((v1 lorentz-vector) (v2 lorentz-vector))
  (make-instance 'lorentz-vector
		 :val-vector
		 (v- (lorentz-val-vector v1)
		     (lorentz-val-vector v2))))

(defmethod mult ((v1 lorentz-vector) (v2 lorentz-vector))
  (minkowski-dot (lorentz-val-vector v1)
		 (lorentz-val-vector v2)))

(defmethod-commutative mult (x (v lorentz-vector))
  (make-instance 'lorentz-vector
		 :val-vector (scal (float x 0d0)
				   (lorentz-val-vector v))))

(defmethod div ((v lorentz-vector) x)
  (make-instance 'lorentz-vector
		 :val-vector
		 (scal (unary-div (float x 0d0))
		       (lorentz-val-vector v))))

;; Reader macro:
(defun lorentz-vector-transformer-reader-macro (stream subchar arg)
  (let ((expr (read stream t)))
    `(make-lorentz-vector ,@expr)))

(set-dispatch-macro-character
 #\# #\l #'lorentz-vector-transformer-reader-macro)

;;; internal use functions:

(defun make-double-float-vector (list &optional (type :row))
  "Constructs a vector of type from list, converting elements into
double-floats"
  (let* ((result-length (length list))
	 (result (make-vector result-length :type type)))
    (loop
       for i from 0 below result-length
       for element in list
       do (setf (vref result i) (float element 0d0))
       finally (return result))))

(defun euclidean-norm (vector)
  "Euclidean norm, from the Pythagorean theorem"
  (sqrt (euclidean-norm2 vector)))

(defun euclidean-norm2 (vector)
  "Euclidean norm^2"
  (dot vector vector))

(defun minkowski-dot (left-vector right-vector)
  "Computes the inner product using the Minkowski metric"
  (flet ((variance-flip-factor (i)
	   (if (zerop i)
	       1
	       -1)))
     (with-accessors ((left-length lisp-matrix::vector-dimension))
	 left-vector
       (with-accessors ((right-length lisp-matrix::vector-dimension))
	   right-vector
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

(defun lorentz-val-vector-spatial (vector)
  "Returns spatial part of the lorentz-vector"
  (with-accessors ((length lisp-matrix::vector-dimension)
		   (orientation lisp-matrix::vector-orientation))
      vector
    (let ((spatial-part (make-vector (1- length) :type orientation)))
      (loop
	 for i from 1 below length
	 do (setf (vref spatial-part (1- i))
		  (vref vector i)))
      spatial-part)))

(defun four-momentum-beta-vector (four-momentum-lorentz-vector)
  "Returns the beta vector from the four-momentum.  Assumes that your
units are chosen such that c is effectively 1 (e.g. GeV/c^2 for mass,
GeV/c for momentum, etc.)"
  (with-accessors ((four-momentum lorentz-val-vector))
      four-momentum-lorentz-vector
    (let ((momentum-vector (lorentz-val-vector-spatial four-momentum))
	  (energy (vref four-momentum 0)))
      (scal (/ energy)
	    momentum-vector))))
