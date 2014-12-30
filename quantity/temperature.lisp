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

(in-package :cl-ana.quantity)

;;;; Temperature units are: :fahrenheit, :celsius, :kelvin, and :rankine

(defun invert-linear (linear-coefs)
  "Yields the coefficients of the inverse of a linear function; i.e. A
+ B*x -> -A/B + (1/B)*x"
  (destructuring-bind (A B)
      linear-coefs
    (list (- (/ A B))
          (/ B))))

(defun compose-linear (linear-coefs1 linear-coefs2)
  "Yields the coefficients of the composition linear1(linear2(x))"
  (destructuring-bind (A1 B1)
      linear-coefs1
    (destructuring-bind (A2 B2)
        linear-coefs2
      (list (+ A1
               (* B1 A2))
            (* B1 B2)))))

(defvar *linear-fn-coefs*
  (make-hash-table :test 'equal))

(defun linear-coefs (xname yname)
  "line-name is a keywordified symbol"
  (gethash (cons (keywordify xname)
                 (keywordify yname))
           *linear-fn-coefs*))

(defun linear-composition-chain (&rest var-chain)
  "Takes a chain of variables and returns the linear function as a
result of sequentially evaluating the linear function required to move
from one variable to the next in the var-chain."
  (reduce (let ((last-var (first var-chain)))
            (lambda (coefs var)
              (let ((new-coefs (linear-coefs last-var var)))
                (setf last-var var)
                (compose-linear new-coefs coefs))))
          (rest var-chain)
          :initial-value (list 0 1)))

(defun linear-funcall (coefs x)
  "Evaluates a linear function with coefficients coefs at the point x"
  (destructuring-bind (A B)
      coefs
    (+ A
       (* B x))))

(defun linear-trans (independent-var dependent-var x)
  "Evaluates the linear transformation (function) which takes the
independent-var to the dependent-var at the point x"
  (linear-funcall (linear-coefs independent-var dependent-var)
                  x))

(defmacro deflinear (xname yname coefs)
  "Defines two functions xname->yname and yname->xname which are the
linear function and its inverse based on coefs."
  (flet ((trans (a b)
           (intern (concatenate 'string (string a) "->" (string b)))))
    (let ((fname (trans xname yname))
          (finvname (trans yname xname))
          (xsym (keywordify xname))
          (ysym (keywordify yname)))
      (with-gensyms (invcs cs)
        `(let* ((,cs ,coefs)
                (,invcs (invert-linear ,cs)))
           (setf (gethash (cons ,xsym ,ysym)
                          *linear-fn-coefs*)
                 ,cs)
           (setf (gethash (cons ,ysym ,xsym)
                          *linear-fn-coefs*)
                 ,invcs))))))

(deflinear celsius fahrenheit
  (list 32
        9/5))

(deflinear celsius kelvin
  (list (/ +T0+ :kelvin)
        1))

(deflinear fahrenheit kelvin
  (linear-composition-chain
   'fahrenheit 'celsius 'kelvin))

(deflinear fahrenheit rankine
  (list 459.67d0
        1))

(deflinear celsius rankine
  (linear-composition-chain
   'celsius 'fahrenheit 'rankine))

(deflinear kelvin rankine
  (linear-composition-chain
   'kelvin 'celsius 'rankine))

(defmacro define-temperature-mult (temp-keyword)
  `(progn
     (defmethod-commutative mult ((x (eql ,temp-keyword)) (y number))
       (with-slots (quantity-scale) x
         (mult (linear-trans ,temp-keyword :kelvin y) :kelvin)))
     (defmethod-commutative mult ((y number) (x (eql ,temp-keyword)))
       (with-slots (quantity-scale) x
         (mult (linear-trans ,temp-keyword :kelvin y) :kelvin)))))

(define-temperature-mult :fahrenheit)
(define-temperature-mult :celsius)
(define-temperature-mult :rankine)

(defmacro define-temperature-quantity (temp-keyword)
  `(defmethod quantity ((x (eql ,temp-keyword)))
     (mult 1 ,temp-keyword)))

(define-temperature-quantity :fahrenheit)
(define-temperature-quantity :celsius)
(define-temperature-quantity :rankine)

;;;; General temperature conversion function:
(defun convert-temperature (temp to-units)
  "Converts the temperature quantity temp into the temperature units to-units.

Note that the result is unitless."
  (let* ((raw-from-units (unit-standard-form (quantity-unit temp)))
         (from-units (first (first raw-from-units))))
    (linear-trans (keywordify from-units) (keywordify to-units)
                  (quantity-scale temp))))
