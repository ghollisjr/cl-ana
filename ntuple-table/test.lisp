(require 'ntuple-table)

(in-package :ntuple-table)

(defparameter *table*
  (create-ntuple-table "/home/ghollisjr/test.dat"
		     (list (cons "x" :int)
			   (cons "y" :double))))

(table-set-field *table* 'x 3)
(table-set-field *table* 'y 5d0)
(table-commit-row *table*)
(table-set-field *table* 'x 4)
(table-set-field *table* 'y 10d0)
(table-commit-row *table*)
(table-close *table*)

(setf *table*
      (open-ntuple-table "/home/ghollisjr/test.dat"
			 (list (cons "x" :int)
			       (cons "y" :double))))

(do-table (row-index *table*)
    ("x"
     "y")
  (print x)
  (print y))
