;;;; generic-math.lisp

;;;; IMPORTANT NOTES
;;;;
;;;; To use the symbols from generic-math in your program, just place
;;;; (gmath:use-gmath <package-designator>) above your code to
;;;; shadowing-import gmath functions into the package you specify.
;;;; It's the best way I've come up with so far for easily replacing
;;;; the cl functions like +, -, ....  My usual routine is to place it
;;;; at the tail end of my package files since it has to do with the
;;;; package.
;;;; 
;;;; Note that you will have to use cl:... from this point on if you
;;;; want access to the shadowed symbols however.
;;;;
;;;; Also: Since common lisp defines the basic number class, but
;;;; doesn't give us a way to extend it: If you want to handle generic
;;;; scalar types in your program, one approach is to just define a
;;;; method for your function which is unspecialized on scalar type
;;;; arguments.  As long as the most basic operations are ultimately
;;;; scalar, this should work.

(in-package :generic-math)

(defun use-gmath (package)
  "shadowing-imports all the exported symbols from gmath into the
  current package"
  (shadowing-use-package :generic-math package))

;; To allow use of incf (since we're touching +)
(defmacro incf (place &optional (delta 1))
  `(setf ,place
	 (add ,place ,delta)))

;; To allow use of decf (since we're touching -)
(defmacro decf (place &optional (delta 1))
  `(setf ,place
	 (sub ,place ,delta)))

;; for all those cases where you want a commutative operator
(defmacro defmethod-commutative (method-name (left-arg right-arg) &body body)
  `(progn
     (defmethod ,method-name (,left-arg ,right-arg)
       ,@body)
     (defmethod ,method-name (,right-arg ,left-arg)
       ,@body)))

;; this macro replaces the oft repeated code for creating a function
;; which calls the generic binary operator to reduce an arbitrary
;; number of arguments.
(defmacro reduce-defun (fname reduce-fname)
  `(defun ,fname (&rest xs)
     (reduce #',reduce-fname xs)))

(reduce-defun + add)

(defun sum (xs)
  "Convience function for summing a list of values (it's reducing +
across them)."
  (reduce #'+ xs))

(defgeneric add (x y)
  (:documentation "Binary addition function"))

(defun - (&rest xs)
  (if (single xs)
      (unary-sub (first xs))
      (reduce #'sub xs)))

(defgeneric sub (x y)
  (:documentation "Binary subtraction function"))

(defgeneric unary-sub (x)
  (:documentation "Unary subtraction function."))

(reduce-defun * mult)

(defgeneric mult (x y)
  (:documentation "Binary multiplication function"))

(defun / (&rest xs)
  (if (single xs)
      (unary-div (first xs))
      (reduce #'div xs)))

(defun protected-/ (&rest xs)
  (if (single xs)
      (protected-unary-div (first xs))
      (reduce #'protected-div xs)))

(defgeneric div (x y)
  (:documentation "Binary division function"))

(defgeneric unary-div (x)
  (:documentation "Unary division function.  Also known as
  multiplicative inversion."))

(defgeneric protected-div (x y &key protected-value)
  (:documentation "Binary division protected from division by zero;
  returns protected-value whenever y is zero")
  (:method (x y &key (protected-value 0))
    (if (zerop y)
	protected-value
	(div x y))))

(defgeneric protected-unary-div (x &key protected-value)
  (:documentation "Protected unary division function.  Returns
  protected-value whenever x is zero.")
  (:method (x &key (protected-value 0))
    (if (zerop x)
	protected-value
	(unary-div x))))

(defgeneric sqrt (x)
  (:documentation "Square root function"))

(defgeneric expt (x y)
  (:documentation "Raises x to the y power"))

(defgeneric exp (x)
  (:documentation "e^x"))

(defgeneric log (x)
  (:documentation "Natural logarithm function"))

(defgeneric sin (x)
  (:documentation "Sine, in radians"))

(defgeneric cos (x)
  (:documentation "Cosine, in radians"))

(defgeneric tan (x)
  (:documentation "Tangent, in radians"))

(defgeneric sec (x)
  (:documentation "Secant, in radians")
  (:method (x)
    (unary-div (cos x))))

(defgeneric csc (x)
  (:documentation "Cosecant, in radians")
  (:method (x)
    (unary-div (sin x))))

(defgeneric cot (x)
  (:documentation "Cotangent, in radians")
  (:method (x)
    (unary-div (tan x))))

(defgeneric sinh (x)
  (:documentation "Hyperbolic sine function"))

(defgeneric cosh (x)
  (:documentation "Hyperbolic cosine function"))

(defgeneric tanh (x)
  (:documentation "Hyperbolic tangent function"))

(defgeneric sech (x)
  (:documentation "Hyperbolic secant function")
  (:method (x)
    (unary-div (cosh x))))

(defgeneric csch (x)
  (:documentation "Hyperbolic cosecant function")
  (:method (x)
    (unary-div (sinh x))))

(defgeneric coth (x)
  (:documentation "Hyperbolic cotangent function")
  (:method (x)
    (unary-div (tanh x))))
