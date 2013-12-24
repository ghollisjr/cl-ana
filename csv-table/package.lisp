;;;; package.lisp

(defpackage #:csv-table 
  (:use :cl
	:table
	:cl-csv
	:iterate)
  (:export :csv-table
	   :make-csv-table
	   :open-csv-table))
