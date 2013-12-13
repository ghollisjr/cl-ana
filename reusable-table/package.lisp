;;;; package.lisp

(defpackage #:reusable-table
  (:use :cl
	:table)
  (:export :wrap-for-reuse))
