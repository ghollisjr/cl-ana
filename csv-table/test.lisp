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

(require 'cl-ana.csv-table)

(in-package :cl-ana.csv-table)

(defparameter *table*
  (create-csv-table "/home/ghollisjr/test.csv"
		  (list "x" "y")))

(table-push-fields *table*
  ("x" 1)
  ("y" 2))

(table-set-field *table* :|x| 3)
(table-set-field *table* :|y| 4)
(table-commit-row *table*)
(table-set-field *table* :|x| 5)
(table-set-field *table* :|y| 6)
(table-commit-row *table*)
(table-close *table*)

(setf *table*
      (open-csv-table "/home/ghollisjr/test.csv"))

(do-table (row-index *table*)
    ("x"
     "y")
  (print |x|)
  (print |y|))

(table-close *table*)
