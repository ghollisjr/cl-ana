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

(in-package :currying)

(defun curry-nth (fn val n)
  "Returns a partially applied fn where argument with index
n (starting from 0) from fn has value val; a generalization of
Alexandria's curry and rcurry."
  (lambda (&rest xs)
    (cond
      ((zerop n)
       (apply fn val xs))
      ((>= n (1- (length xs)))
       (apply fn (append xs (list val))))
      (t
       (apply fn
              (nreverse
               (let ((result ()))
                 (loop
                    for x in xs
                    for i from 0
                    do (progn
                         (when (equal i n)
                           (push val result))
                         (push x result)))
                 result)))))))

(defun curry-nth-parallel (fn &rest val-indices)
  "Curries each value at each index in alist val-indices in parallel
to fn.  e.g. (curry-nth fn (cons 1 . 0)) <==> (curry fn 1)"
  (let ((result fn)
        (sorted-val-indices
         (sort (copy-list val-indices)
               #'>
               :key #'cdr)))
    (loop
       for (val . index) in sorted-val-indices
       for i from 0
       do (setf result
                (curry-nth result val index)))
    result))
