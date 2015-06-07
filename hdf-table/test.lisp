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

(require 'cl-ana.hdf-table)

(in-package :cl-ana.hdf-table)

(defun hdf-type-test ()
  (hdf-type->typespec
   (typespec->hdf-type
    '(:compound
      ("x" . :double)
      ("y" . :int)
      ("xs" . (:array :double 5))
      ("ys" . (:compound
	       ("t" . :int)))))))

(defun hdf-type-test2 ()
  (hdf-type->typespec
   (typespec->hdf-type
    '(:compound
      ("x" . :double)
      ("y" . :int)
      ("xs" . (:compound
	       ("fs" . (:array :double 5))))
      ("ys" . (:compound
	       ("t" . :int)))))))

(defvar *table* nil)

(defvar *hdf-typespec*)

(defvar *table-typespec*)

(defvar *hdf-size*)

(defvar *cstruct-size*)

(defvar *hdf-file*)

(defun hdf-read-data-test ()
  (with-open-hdf-file (file "/home/ghollisjr/phys/research/phd/ana/hdffile.h5"
			    :direction :input)
    (setf *table* (open-hdf-table file "/h10"))
    (setf *hdf-file* file)
    (let ((result 0)
	  (last-row-index 0))
      (setf *table-typespec* (typed-table->typespec *table*))
      (setf *hdf-size* (h5tget-size (hdf-table-row-type *table*)))
      (setf *hdf-typespec* (hdf-type->typespec (hdf-table-row-type *table*)))
      (setf *cstruct-size* (foreign-type-size (typed-table-row-cstruct *table*)))
      (do-table (row-index *table*)
	  ("gpart")
	(incf result gpart)
	(setf last-row-index row-index))
      (format t "last-row-index: ~a~%" last-row-index)
      result)))

(defun hdf-write-table-test ()
  (with-open-hdf-file (file
		       "/home/ghollisjr/hdfwork/test.h5"
		       :direction :output
		       :if-exists :supersede)
    (let* ((table
            (create-hdf-table file
                            "/test"
                            (list (cons "x" :int)
                                  (cons "y" :float)))))
      ;;(print (typed-table-type-map table))
      (dotimes (i 1000000)
	(table-set-field table :|x| i)
	(table-set-field table :|y| (sqrt i))
	(table-commit-row table))
      (table-close table))))

(defun hdf-read-test ()
  (when *table*
    (table-close *table*))
  (setf *table* (open-hdf-table-chain (list "/home/ghollisjr/hdfwork/test.h5") "/test"))
  (let ((sum 0))
    (do-table (row-index *table*)
        ("x" "y")
      (incf sum |x|)
      (incf sum |y|))
    (print sum)))

(defun hdf-table-test ()
  (with-open-hdf-file (outfile "/home/ghollisjr/hdfwork/outfile.h5"
			       :direction :output
			       :if-exists :supersede)
    (let ((output-table (create-hdf-table outfile "/output-dataset"
                                        (zip (list "x" "y")
                                             (list :int :float))))
          (input-table
           (open-hdf-table-chain (list "/home/ghollisjr/hdfwork/test.h5") "/test")))
      ;;(open-hdf-table infile "/test")))
      (do-table (row-index input-table)
          ("x" "y")
        ;; (format t "~a ~a; ~a ~a~%"
        ;;         row-index (sqrt row-index)
        ;;         x y)
        
        (when (> row-index (hdf-table-chain-nrows input-table))
          (when (zerop (mod row-index 1000))
            (print row-index)))
        (when (<= 25 y 30)
          (table-push-fields output-table
            x
            y)))
      (table-close output-table)
      (table-close input-table))))

(defun hdf-typed-table-test ()
  (with-open-hdf-file (outfile "/home/ghollisjr/hdfwork/outfile.h5"
			       :direction :output
			       :if-exists :supersede)
    (let ((output-table (create-hdf-table outfile "/output-dataset"
					(zip (list "x" "y")
					     (list :int :float))))
	  (input-table
	   (open-hdf-table-chain (list "/home/ghollisjr/hdfwork/test.h5") "/test")))
      (do-table (row-index input-table)
	  ("x" "y")
	(when (<= 25 y 30)
	  (table-set-field output-table :x x)
	  (table-set-field output-table :y y)
	  (table-commit-row output-table)))
      (table-close output-table)
      (table-close input-table))))

(defun array-test ()
  (with-open-hdf-file (outfile "/home/ghollisjr/hdfwork/array.h5"
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
    (let* ((table
            (create-hdf-table
             outfile "/array"
             (list (cons "x"
                         (list :array :float 4))))))
      (loop
         for i below 30
         do (progn
              (table-push-fields table
                (x
                 (vector (float i 0f0) (sqrt (float i 0f0)) (float i 0f0)
                         (sqrt (float i)))))))
      (table-close table))))
