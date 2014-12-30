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
;;;; generic-math.lisp

;;;; IMPORTANT NOTES
;;;;
;;;; To use the symbols from generic-math in your program, just place
;;;; (cl-ana.gmath:use-gmath <package-designator>) above your code to
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

;;;; TODO:
;;;; * Implement the inverse trig functions

(in-package :cl-ana.generic-math)

(defun use-gmath (package)
  "shadowing-imports all the exported symbols from gmath into the
  current package"
  (shadowing-use-package :cl-ana.generic-math package))

(defvar *gmath-generic-map* (make-hash-table :test 'equal)
  "Hash table mapping generic function symbols to the argument
  specification for the function.")

;; Macro for defining new gmath generics
(defmacro defmath (fname (&rest args) &body body)
  "Defines a generic function for use in generic-math.  Necessary to
allow for programmatically generated methods of certain mathematical
types.  Can use body just like with defgeneric to specify methods
etc."
  `(progn
     (setf (gethash ',fname *gmath-generic-map*)
           ',args)
     (defgeneric ,fname ,args ,@body)))

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

;; To allow use of incf (since we're touching +)
(defmacro incf (place &optional (delta 1))
  `(setf ,place
	 (add ,place ,delta)))

;; To allow use of decf (since we're touching -)
(defmacro decf (place &optional (delta 1))
  `(setf ,place
	 (sub ,place ,delta)))

(reduce-defun + add)

(defun sum (xs)
  "Convenience function for summing a list of values (it's reducing +
across them)."
  (reduce #'+ xs))

(defun product (xs)
  "Convenience function for multiplying a list of values"
  (reduce #'* xs))

(defmath add (x y)
  (:documentation "Binary addition function"))

(defun - (&rest xs)
  (if (single xs)
      (unary-sub (first xs))
      (reduce #'sub xs)))

(defmath sub (x y)
  (:documentation "Binary subtraction function"))

(defmath unary-sub (x)
  (:documentation "Unary subtraction function."))

(reduce-defun * mult)

(defmath mult (x y)
  (:documentation "Binary multiplication function"))

(defun / (&rest xs)
  (if (single xs)
      (unary-div (first xs))
      (reduce #'div xs)))

(defun protected-/ (&rest xs)
  (if (single xs)
      (protected-unary-div (first xs))
      (reduce #'protected-div xs)))

(defmath div (x y)
  (:documentation "Binary division function"))

(defmath unary-div (x)
  (:documentation "Unary division function.  Also known as
  multiplicative inversion."))

(defmath protected-div (x y &key protected-value)
  (:documentation "Binary division protected from division by zero;
  returns protected-value whenever y is zero")
  (:method (x y &key (protected-value 0))
    (if (zerop y)
	protected-value
	(div x y))))

(defmath protected-unary-div (x &key protected-value)
  (:documentation "Protected unary division function.  Returns
  protected-value whenever x is zero.")
  (:method (x &key (protected-value 0))
    (if (zerop x)
	protected-value
	(unary-div x))))

(defmath sqrt (x)
  (:documentation "Square root function"))

(defmath expt (x y)
  (:documentation "Raises x to the y power"))

(defmath exp (x)
  (:documentation "e^x"))

(defmath log (x)
  (:documentation "Natural logarithm function"))

(defmath sin (x)
  (:documentation "Sine, in radians"))

(defmath cos (x)
  (:documentation "Cosine, in radians"))

(defmath tan (x)
  (:documentation "Tangent, in radians"))

(defmath sec (x)
  (:documentation "Secant, in radians")
  (:method (x)
    (unary-div (cos x))))

(defmath csc (x)
  (:documentation "Cosecant, in radians")
  (:method (x)
    (unary-div (sin x))))

(defmath cot (x)
  (:documentation "Cotangent, in radians")
  (:method (x)
    (unary-div (tan x))))

(defmath sinh (x)
  (:documentation "Hyperbolic sine function"))

(defmath cosh (x)
  (:documentation "Hyperbolic cosine function"))

(defmath tanh (x)
  (:documentation "Hyperbolic tangent function"))

(defmath sech (x)
  (:documentation "Hyperbolic secant function")
  (:method (x)
    (unary-div (cosh x))))

(defmath csch (x)
  (:documentation "Hyperbolic cosecant function")
  (:method (x)
    (unary-div (sinh x))))

(defmath coth (x)
  (:documentation "Hyperbolic cotangent function")
  (:method (x)
    (unary-div (tanh x))))
