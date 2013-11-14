;;;; package.lisp

(defpackage #:rread-table
  (:use :cl
	:table)
  (:export :rread-table
	   :rread-table-read-row-index
	   :rread-table-nrows
	   :rread-table-get-row-field))
