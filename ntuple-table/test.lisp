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
