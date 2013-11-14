;;; Test results: It looks like the slowness in reading the data files
;;; is coming from the h5dread function, not from the LISP code!!!
;;; This means that I may still be able to do my analysis in common
;;; lisp after all.  The real issue seems to be the chunk size, as I
;;; currently have this set to 1 element per chunk.  This was my first
;;; dummy value, but now I think I should test various chunk sizes
;;; with C++ code and then use the proper chunk settings in my data
;;; files.
;;;
;;; I'll have to change the way I implement hdf-table to allow for a
;;; chunk buffer which stores multiple rows; shouldn't be too hard.

(in-package :hdf-table)

(defvar *table*)

(setf *table*
      (make-instance 'hdf-table :column-names (list "x" "y" "z") :column-specs (list :int :float (list :compound (cons "weight" :double) (cons "direction vector" (list :array :double 1 (list 20)))))))

;; (defun hdf-table-test ()
;;   (typespec-make-hdf-type (table-make-typespec *table*)))

(defun hdf-type-test ()
  (hdf-type-make-typespec
   (typespec-make-hdf-type
    '(:compound
      ("x" . :double)
      ("y" . :int)
      ("xs" . (:array :double 1 (5)))
      ("ys" . (:compound
	       ("t" . :int)))))))

(defun hdf-type-test2 ()
  (hdf-type-make-typespec
   (typespec-make-hdf-type
    '(:compound
      ("x" . :double)
      ("y" . :int)
      ("xs" . (:compound
	       ("fs" . (:array :double 1 (5)))))
      ("ys" . (:compound
	       ("t" . :int)))))))

(defvar *table*)

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
      (setf *table-typespec* (table-make-typespec *table*))
      (setf *hdf-size* (h5tget-size (hdf-table-row-type *table*)))
      (setf *hdf-typespec* (hdf-type-make-typespec (hdf-table-row-type *table*)))
      (setf *cstruct-size* (foreign-type-size (hdf-table-row-cstruct *table*)))
      (dotable (row-index *table*)
	(incf result /gpart)
	(setf last-row-index row-index))
      (format t "last-row-index: ~a~%" last-row-index)
      result)))

(defun hdf-write-table-test ()
  (with-open-hdf-file (file
		       "/home/ghollisjr/hdfwork/test.h5"
		       :direction :output
		       :if-exists :supersede)
    (let* ((table (make-hdf-table file "/test" (list (cons "x" :int) (cons "y" :float)))))
      (dotimes (i 1000000)
	(table-set-field table 'x i)
	(table-set-field table 'y (sqrt i))
	(table-commit-row table))
      (table-close table))))

(defun hdf-read-test ()
  (setf *table* (open-hdf-table-chain (list "/home/ghollisjr/hdfwork/test.h5") "/test"))
  (dotable (row-index *table*) (format t "x: ~a, y: ~a~%" /x /y)))

(defun hdf-table-test ()
  (with-open-hdf-file (outfile "/home/ghollisjr/hdfwork/outfile.h5"
			       :direction :output
			       :if-exists :supersede)
    (let ((output-table (make-hdf-table outfile "/output-dataset"
					(zip (list "x" "y")
					     (list :int :float))))
	  (input-table
	   (open-hdf-table-chain (list "/home/ghollisjr/hdfwork/test.h5") "/test")))
      (dotable (row-index input-table)
	(when (<= 25 /y 30)
	  (table-set-field output-table 'x /x)
	  (table-set-field output-table 'y /y)
	  (table-commit-row output-table)))
      (table-close output-table)
      (table-close input-table))))
