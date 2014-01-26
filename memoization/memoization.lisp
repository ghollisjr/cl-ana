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
;;;; memoization.lisp

;;; Memoized functions remember the previous calls of the function and
;;; look-up the return value from the last time the function was
;;; called.
;;;
;;; This implementation uses hash tables to store the previous call values.
;;;
;;; I am still unsure whether or not to expose access to the
;;; memoization hash table, at the moment it is not exposed.

(in-package :memoization)

(defvar *memoized-map* (make-hash-table :test 'equal)
  "Hash table mapping each memoized function to its value hash
  table.")

(defun get-memo-map (memo-fn)
  "Returns the memoized function's value hash table."
  (gethash memo-fn *memoized-map*))

(defun reset-memo-map (memo-fn)
  "Resets the memoization hash table for memo-fn"
  (clrhash (gethash memo-fn *memoized-map*)))

(defmacro defun-memoized (function-name arg-list &body body)
  "Macro for defining a memoized function"
  (with-gensyms (memo-hash-table result memoed-values)
    `(let ((,memo-hash-table
            (make-hash-table :test 'equal)))
       (defun ,function-name ,arg-list
	 (let ((,memoed-values
                (multiple-value-list (gethash (list ,@arg-list)
                                              ,memo-hash-table))))
	   (if (second ,memoed-values)
	       (first ,memoed-values)
	       (let ((,result (progn ,@body)))
		 (setf (gethash (list ,@arg-list) ,memo-hash-table) ,result)
		 ,result))))
       (setf (gethash #',function-name *memoized-map*)
             ,memo-hash-table))))
