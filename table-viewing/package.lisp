;;;; package.lisp

(defpackage #:table-viewing
  (:use #:cl
	#:alexandria
        #:string-utils
        #:table
        #:histogram
        #:plotting)
  (:export :table-view))

(gmath:use-gmath :table-viewing)
