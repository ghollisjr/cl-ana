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
(in-package :quantity)

;;;; Temperature units are: :farenheit, :celsius, :kelvin, and :rankine

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

;; (defun ,fname (x)
;;   (+ (first ,cs)
;;      (* (second ,cs) x)))
;; (defun ,finvname (x)
;;   (+ (first ,invcs)
;;      (* (second ,invcs) x)))

;;;; Required pairings:
;;;; celsius farenheit
;;;; celsius kelvin
;;;; farenheit kelvin
;;;; farenheit rankine
;;;; celsius rankine
;;;; kelvin rankine

(deflinear celsius farenheit
  (list (* 32 :farenheit)
        (* 9/5 :farenheit (/ :celsius))))

(deflinear celsius kelvin
  (list +T0+
        (/ :kelvin :celsius)))

(deflinear farenheit kelvin
  (linear-composition-chain
   'farenheit 'celsius 'kelvin))

(deflinear farenheit rankine
  (list (* 459.67d0 :rankine)
        (/ :rankine
           :farenheit)))

(deflinear celsius rankine
  (linear-composition-chain
   'celsius 'farenheit 'rankine))

(deflinear kelvin rankine
  (linear-composition-chain
   'kelvin 'celsius 'rankine))

;;;; General temperature conversion function:
(defun convert-temperature (temp to-units)
  "Converts the temperature quantity temp into the temperature units to-units.

Note that temp must have units, so (* 0 :celsius) would not work, one
should instead use #q(0 :celsius)."
  (let* ((raw-from-units (unit-standard-form (quantity-unit temp)))
         (from-units (first (first raw-from-units))))
    (linear-trans (keywordify from-units) (keywordify to-units) temp)))
