;;;; package.lisp

(defpackage #:table-viewing
  (:use #:cl
	#:alexandria
        #:list-utils
        #:string-utils
        #:macro-utils
        #:table
        #:histogram
        #:plotting)
  (:export :table-view
           :table-easy-view))

(gmath:use-gmath :table-viewing)
