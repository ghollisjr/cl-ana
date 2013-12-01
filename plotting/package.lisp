;;;; package.lisp

(defpackage #:plotting
  (:use :cl
        :gnuplot-i-cffi
	:map
        :string-utils
        :list-utils
        :histogram
        :tensor)
  (:export :*gnuplot-session*
           :reset-gnuplot-session
           :titled
           :page
	   :plot
	   :plot2d
	   :plot3d
	   :line
           :generate-cmd
           ;; page functions
           :draw
           :page-next-id
           :page-id
           :page-dimensions
           :page-scale
           :page-plots
           :page-layout
           :page-type
           :page-add-plot
           ;; plot functions
           :plot-lines
           :plot-legend
           :plot-add-line
           :plot2d-x-range
           :plot2d-y-range
           ;; line functions
           :line-style
           :line-color
           :line-plot-arg
           :line-options
           ;; data-line functions
           :data-line-data
           ;; analytic-line functions
           :analytic-line-fn-string
           ;; legend functions
           :legend-contents
           :legend-update-strategy
           :legend-update
           ;; ease of use
           :quick-multidraw
           :quick-draw
           :make-line))

(gmath:use-gmath :plotting)
