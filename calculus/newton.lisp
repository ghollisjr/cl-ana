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
                     (/ (funcall func x)
                        (funcall df x))))
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
