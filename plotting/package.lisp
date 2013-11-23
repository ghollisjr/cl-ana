;;;; package.lisp

(defpackage #:plotting
  (:use :cl
	:map)
  (:export :page
	   :plot
	   :plot2d
	   :plot3d
	   :graph
	   :line))
