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

(in-package :cl-ana.hash-table-utils)

(defun hash-table->alist (hash-table)
  (loop
     for k being the hash-keys of hash-table
     for v being the hash-values of hash-table
     collect (cons k v)))

(defun hash-keys (hash-table)
  (mapcar #'car (hash-table->alist hash-table)))

(defun hash-values (hash-table)
  (mapcar #'cdr (hash-table->alist hash-table)))

(defun hmap (fn hash-table)
  "hmap is a more functional version of maphash; instead of returning
nil and relying on mutable state, hmap returns a new hash table with
the same keys but mapped values in the resulting hash table.

fn is a function which takes a key and value from hash-table and
returns an updated value for the resulting hash table."
  (let ((result (make-hash-table
                 :test (hash-table-test hash-table))))
    (maphash (lambda (k v)
               (setf (gethash k result)
                     (funcall fn k v)))
             hash-table)
    result))

(defun alist->hash-table (alist &optional (test 'eql))
  (let ((result (apply #'make-hash-table
                       (when test
                         (list :test test)))))
    (loop
       for (k . v) in alist
       do (setf (gethash k result)
                v))
    result))

;; not sure if this is the same as the one alexandria provides
(defun copy-hash-table (ht)
  "Returns a new copy of ht"
  (let ((result (make-hash-table :test (hash-table-test ht))))
    (maphash (lambda (k v)
               (setf (gethash k result)
                     v))
             ht)
    result))
