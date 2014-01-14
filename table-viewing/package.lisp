;;;; package.lisp

(defpackage #:table-viewing
  (:use #:cl
	#:alexandria
        #:string-utils
        #:macro-utils
        #:table
        #:histogram
        #:plotting)
  (:export :table-view
           :table-easy-view))

(gmath:use-gmath :table-viewing)
