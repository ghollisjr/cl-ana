;;;; package.lisp

(defpackage #:ntuple-table
  (:use :cl
	:cffi
	:table
        :typed-table
	:typespec)
  (:export :open-ntuple-table
	   :create-ntuple-table))
