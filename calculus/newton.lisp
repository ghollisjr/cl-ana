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

(in-package :cl-ana.calculus)

(defun newton (fn guess &key
                          (step-scale 1)
                          (value 0)
                          (maxtries 50)
                          (prec 1d-4)
                          (diff-prec 1d-9))
  "1-D Newton's method for solving an equation.

If the method converges, returns the values of x_sol, fn(x_sol), and
the number of iterations it took to converge.

If the method does not converge, returns the values of nil and the
number of iterations on failure.

value is the value that fn should evaluate to.

maxtries is the maximum number of iterations to attempt a solution.

prec is the precision in the residue, below which convergence is assumed.

diff-prec is the precision to use in calculating the derivative of fn
via diff."
  (let ((df (diff fn :prec diff-prec))
        (func (lambda (x) (- (funcall fn x)
                             value))))
    (do ((x guess (- x
                     (* step-scale
                        (/ (funcall func x)
                           (funcall df x)))))
         (i 0 (1+ i))
         (difference (* 10 prec) (abs (funcall func x))))
        ((or (<= difference prec)
             (>= i maxtries))
         (if (> difference prec)
             (values nil i)
             (values x
                     (funcall fn x)
                     i))))))

(defun invert (fn &key
                    (guess-fn (constantly 1))
                    (newton-prec 1d-4)
                    (diff-prec 1d-9))
  "Inverts a function numerically using Newton's method.  Returns the
inverse function using the precision parameters given.

guess-fn is a function which evaluates to a guess value for each input
value to the inverse function to use in Newton's method.

newton-prec is the precision argument to give to Newton's method for
the precision in the residue of the result.

diff-prec is the precision to give to diff during Newton's method."
  (lambda (x)
    (newton fn (funcall guess-fn x)
            :value x
            :prec newton-prec
            :diff-prec diff-prec)))

;; I've kept a few pieces of code in this function that aren't used
;; because more investigration is needed as to which strategy is best
;; for general use.
;;
;; LU decomposition is used to either invert the derivative matrix or
;; solve a matrix equation involving that matrix.  Inversion of the
;; matrix provides more stability as the same matrix is used to
;; multiply as many inputs as desired, but lu-solve can be more
;; efficient depending on the derivative matrix.
(defun multinewton (fn guess &key
                               (step-scale 1)
                               (value 0)
                               (maxtries 50)
                               (prec 1d-4)
                               (diff-prec 1d-9)
                               (metric :norm2))
  "Multi-dimensional Newton's method for solving a system of equations.

Dimensionality of inputs and outputs is handled as per multidiff.
Note that the input and output dimensionality must match as this
attempts to find an exact solution to a system of N equations in N
variables.

If the method converges, returns the values of x_sol, fn(x_sol), and
the number of iterations it took to converge.

If the method does not converge, returns the values of nil and the
number of iterations on failure.

value is the value that fn should evaluate to.  Note that scalar
values are interpreted as vectors with all elements equal to the
scalar value.  This is particularly convenient for the default value
of 0.

maxtries is the maximum number of iterations to attempt a solution.

prec is the precision in the residue, below which convergence is assumed.

diff-prec is the precision to use in calculating the derivative of fn
via diff.

metric can be one of:

:max for maximum of differences
:norm for euclidean-norm of difference vector
:norm2 for euclidean-norm2 of difference vector"
  (let* ((mfn (case metric
                (:max
                 (lambda (x)
                   (reduce #'max
                           (map 'list #'abs x))))
                (:norm
                 (lambda (x)
                   (sqrt
                    (reduce #'+
                            (map 'list (lambda (y)
                                         (expt y 2))
                                 x)))))
                (:norm2
                 (lambda (x)
                   (reduce #'+
                           (map 'list (lambda (y)
                                        (expt y 2))
                                x))))))
         (df (multidiff fn :prec diff-prec))
         (inverse (lambda (v)
                    (lu-invert
                     (lisp-2d-array->tensor
                      (funcall df v)))))
         (func (lambda (x) (- (funcall fn x)
                              value)))
         (solver (lambda (v)
                   (lu-solve (lisp-2d-array->tensor
                              (funcall df v))
                             (coerce (funcall func v) 'list))))
         )
    (do ((x guess (- x
                     ;; (funcall solver x)
                     ;; (lu-solve (lisp-2d-array->tensor
                     ;;            (funcall df x))
                     ;;           (coerce (funcall func x) 'list))
                     (* step-scale
                        (matrix-mult (funcall inverse x)
                                     (funcall func x)))
                     ))
         (i 0 (1+ i))
         (difference (* 10 prec)
                     (funcall mfn (funcall func x))))
        ((or (<= difference prec)
             (>= i maxtries))
         (if (> difference prec)
             (values nil i)
             (values x
                     (funcall fn x)
                     i))))))
