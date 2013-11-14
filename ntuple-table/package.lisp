;;;; package.lisp

(defpackage #:ntuple-table
  (:use :cl
	:cffi
	:table
	:typespec)
  (:export :open-ntuple-table
	   :make-ntuple-table)
  (:shadow :row))
