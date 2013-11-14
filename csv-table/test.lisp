(require 'csv-table)

(in-package :csv-table)

(defparameter *table*
  (make-csv-table "/home/ghollisjr/test.csv"
		  (list "x" "y")))

(table-set-field *table* 'x 3)
(table-set-field *table* 'y 4)
(table-commit-row *table*)
(table-set-field *table* 'x 5)
(table-set-field *table* 'y 6)
(table-commit-row *table*)
(table-close *table*)

(setf *table*
      (open-csv-table "/home/ghollisjr/test.csv"))

(dotable (row-index *table*)
  (print /x)
  (print /y))
