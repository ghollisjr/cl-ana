;;;; lisp-matrix-utils.lisp

(in-package :lisp-matrix-utils)

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
