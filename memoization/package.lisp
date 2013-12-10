;;;; package.lisp

(defpackage #:memoization
  (:use #:cl
        #:alexandria)
  (:export :defun-memoized))
