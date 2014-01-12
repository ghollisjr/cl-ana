;;;; package.lisp

(defpackage #:table 
  (:use #:cl
	#:list-utils
	#:macro-utils
	#:string-utils
	#:functional-utils
	#:alexandria
        #:table
        #:reusable-table
        #:plotting)
  (:export ))

(gmath:use-gmath :table-viewing)
