;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013-2015 Gary Hollis
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

(require 'cl-ana.makeres)

(in-package :cl-ana.makeres)

(in-project logres-test)

(set-project-path "~/logres-test/")

(defres source
  (list (list :x 1)
        (list :x 2)
        (list :x 3)))

(defres vector
  (vector 1 2 3 4 5))

(defres (mean x)
  (/ (sum (mapcar (lambda (x)
                    (getf x :x))
                  (res source)))
     (length (res source))))

(defres hash-table
  (let ((hash-table (make-hash-table :test 'equal)))
    (setf (gethash 'x hash-table)
          'x)
    hash-table))

(defres nested
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash 'hash-table ht)
          (res hash-table))
    ht))
