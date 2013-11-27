;;;; package.lisp

(defpackage #:plotting
  (:use :cl
        :gnuplot-i-cffi
	:map
        :string-utils)
  (:export :*gnuplot-session*
           :reset-gnuplot-session
           :page
	   :plot
	   :plot2d
	   :plot3d
	   :graph
	   :line))
