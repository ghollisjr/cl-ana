;;;; package.lisp

(defpackage #:memoization 
  (:use #:cl
        #:alexandria)
  (:export :defun-memoized
           :get-memo-map
           :reset-memo-map))
