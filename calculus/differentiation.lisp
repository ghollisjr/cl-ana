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

(defun diff (fn &key (prec 1d-9))
  "Returns the one-sided numerical derivative of a function."
  (lambda (x) (/ (- (funcall fn (+ x prec))
                    (funcall fn x))
                 prec)))

(defun multidiff (fn
                  &key
                    (prec 1d-9))
  "Returns function to compute the matrix of derivatives of a
many-valued function of multiple inputs.  Assumes fn uses the same
input and output sequence types.  Return type of the generated
function is a 2-D array."
  (lambda (v)
    (let* ((type (type-of v))
           (len (length v))
           (deltas
            (loop
               for i below len
               collecting
                 (let* ((s (make-sequence type len
                                          :initial-element 0d0)))
                   (setf (elt s i) prec)
                   s)))
           (base (funcall fn v))
           (outlen (length base))
           (result (make-array (list outlen len)
                               :element-type 'double-float)))
      (loop
         for j below len
         for delta in deltas
         do
           (let* ((y (funcall fn (+ v delta)))
                  (diff (/ (- y base)
                           prec)))
             (loop
                for i below outlen
                do (setf (aref result i j)
                         (elt diff i)))))
      result)))
