;;;; functions.lisp

(in-package :fitting)

;;;; Defines various fit functions for use with the least squares
;;;; fitting function

(defun polynomial (params x)
  "A general single-dimensional polynomial.  Gets its order from the
size of params; assumes parameters are given from least order term to
greatest.

Example: A line f(x) = A + B*x ==> (polynomial (list A B) x)"
  (labels ((polynomial-worker (params x xtmp nparams result)
	     (if (zerop nparams)
		 result
		 (polynomial-worker (rest params)
				    x
				    (* x xtmp)
				    (1- nparams)
				    (+ result (* (first params) xtmp))))))
    (polynomial-worker params x 1 (length params) 0)))

(defun exponential (params x)
  "Exponential fitting function:
f(x) = A * exp(B * x) ==> (exponential (list A B) x)"
  (let ((A (first params))
	(B (second params)))
    (* A
       (exp (* B
	       x)))))

(defun power (params x)
  "Power fitting function:
f(x) = A * B^x ==> (power (list A B) x)"
  (let ((A (first params))
	(B (second params)))
    (* A
       (expt B x))))

(defun logarithm (params x)
  "A logarithmic fit function:
f(x) = A + log(B * x) ==> (logarithm (list A B) x)"
  (let ((A (first params))
	(B (second params)))
    (+ A
       (log (* B
	       x)))))

(defun sinusoid (params x)
  "A sinusoidal fit function:
f(x) = A * sin(omega*x + phi) ==> (sinusoid (list A omega phi) x)"
  (let ((A (first params))
	(omega (second params))
	(phi (third params)))
    (* A
       (sin (+ (* omega
		  x)
	       phi)))))

(defun gaussian (params x)
  "Gaussian fit function:
f(x) = A/(sigma*sqrt(2*pi)) * exp(-((x-mu)/sigma)^2/2)
  ==> (gaussian (list A mu sigma))"
  (let ((A (first params))
	(mu (second params))
	(sigma (third params)))
    (* (/ A
	  (* sigma
	     (sqrt (* 2
		      pi))))
       (exp (- (/ (expt (/ (- x mu)
			   sigma)
			2)
		  2))))))
