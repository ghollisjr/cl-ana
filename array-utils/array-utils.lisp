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

(in-package :cl-ana.array-utils)

;; Looping over array
(defmacro for-array (binding dimensions &body body)
  "Iterates over every possible index for an array with supplied
dimensions.  If binding is an atom, then the index list will be set to
that variable as a list.  If it is a list of symbols, then each symbol
will be bound to its corresponding element from the Cartesian product
element."
  (alexandria:with-gensyms (sets dims setdims nsets
                                 index
                                 continue
                                 incr dim)
    `(let* ((,dims ,dimensions)
            (,nsets (if (atom ,dims)
                        1
                        (length ,dims)))
            (,setdims (if (atom ,dims)
                          (vector ,dims)
                          (map 'vector #'identity ,dims)))
            (,index (make-array ,nsets :initial-element 0))
            (,continue t))
       (loop
          while ,continue
          do
          ;; execute body
            ,(if (atom binding)
                 `(let ((,binding (map 'list #'identity
                                       ,index)))
                    ,@body)
                 `(destructuring-bind ,binding
                      (map 'list #'identity
                           ,index)
                    ,@body))
          ;; iterate
            (let* ((,incr t)
                   (,dim 0))
              (loop
                 while ,incr
                 do
                   (if (>= ,dim ,nsets)
                       (progn
                         (setf ,incr nil)
                         (setf ,continue nil))
                       (progn
                         (incf (aref ,index ,dim))
                         (if (>= (aref ,index ,dim)
                                 (aref ,setdims ,dim))
                             (progn
                               (setf (aref ,index ,dim) 0)
                               (incf ,dim))
                             (setf ,incr nil))))))))))

(defun map-array (fn array)
  "Map for arrays.  fn should be of the form (lambda (value &rest
indices)...)"
  (let* ((result (make-array (array-dimensions array))))
    (for-array indices (array-dimensions array)
      (setf (apply #'aref result indices)
            (apply fn
                   (apply #'aref array indices)
                   indices)))
    result))
