;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2021 Gary Hollis
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

;;;; Runge-Kutta 4th-order solver in Lisp
(defun rk4 (dy/dx y0 x0 xN N)
  "Applies the 4th-order Runge Kutta method (RK4) to solve dy/dx =
f(x,y) where y(x0) = y0.  xN is the last value of x, and N is the
number of x values to find approximations for y(x).

dy/dx should be a function (lambda (x y) ...) returning dy/dx for
given (x,y).

y can have numerical values or sequence values."
  (cond
    ((<= N 0) NIL)
    ((= N 1) (list (cons x0 y0)))
    (t
     (let* ((result (list (cons x0 y0)))
            (x x0)
            (y y0)
            (h (/ (- xN x0) (1- N)))
            (h2 (* 0.5d0 h))
            (h6 (/ h 6d0)))
       (labels ((stepfn ()
                  (let* ((k1 (funcall dy/dx x y))
                         (k2 (funcall dy/dx
                                      (+ x h2)
                                      (+ y (* h2 k1))))
                         (k3 (funcall dy/dx
                                      (+ x h2)
                                      (+ y (* h2 k2))))
                         (k4 (funcall dy/dx
                                      (+ x h)
                                      (+ y (* h k3)))))
                    (incf y
                          (* h6
                             (+ k1
                                (* 2d0 k2)
                                (* 2d0 k3)
                                k4)))
                    (incf x h)
                    (push (cons x y) result))))
         (loop
            for i below (1- N)
            do (stepfn))
         (nreverse result))))))
