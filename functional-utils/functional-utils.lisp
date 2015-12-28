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

(in-package :cl-ana.functional-utils)

(defun flip (f)
  "Takes a function of two parameters and creates a new function which
  takes the same parameters in reverse order; particularly useful in
  conjunction with reduce."
  (lambda (x y) (funcall f y x)))

(defun splice (xsplit fn1 fn2)
  "Splices two functions togother at xsplit.  If either function is
nil, returns the other function or nil if both are nil."
  (cond
    ((and fn1
          fn2)
     (lambda (x)
       (if (<= x xsplit)
           (funcall fn1 x)
           (funcall fn2 x))))
    (fn1 fn1)
    (fn2 fn2)
    (t nil)))

;; (defun compose (&rest fs)
;;   "Arbitrary number of functions composed"
;;   (let ((f (first fs)))
;;     (if (= 1 (length fs))
;; 	(first fs)
;; 	(lambda (&rest xs)
;; 	    (funcall f (apply (apply #'compose (rest fs)) xs))))))

(defun to-pair-function (f)
  "Makes f apply to a pair instead of two arguments"
  (lambda (x) (funcall f (car x ) (cdr x))))

(defun lfuncall (fn &rest args)
  "Applies a function which takes a single list argument to an
arbitrary number of arguments."
  (funcall fn args))

;; Didn't really know where to put these, but they are higher-order
;; functions:

(defun min-by (x y &key (key #'identity))
  (if (< (funcall key x)
         (funcall key y))
      x
      y))

(defun minimum (xs &key (key #'identity))
  (when xs
    (reduce (lambda (x y) (min-by x y :key key))
            xs)))

(defun max-by (x y &key (key #'identity))
  (if (> (funcall key x)
         (funcall key y))
      x
      y))

(defun maximum (xs &key (key #'identity))
  (when xs
    (reduce (lambda (x y) (max-by x y :key key))
            xs)))

(defun iterate (fn initial count)
  "Calls a function on its own result for count iterations.  Returns
the final result."
  (do* ((i 1 (1+ i))
        (fnval initial (funcall fn fnval)))
       ((> i count) fnval)))

(defun iterate-collect (fn initial count)
  "Calls a function on its own result for count iterations.  Returns
the full list of results."
  (do* ((i 1 (1+ i))
        (fnval initial (funcall fn fnval))
        (result nil (cons fnval result)))
       ((> i count) (nreverse result))))
