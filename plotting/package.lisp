;;;; package.lisp

(defpackage #:plotting
  (:use :cl
        :error-propogation
        :gnuplot-i-cffi
	:map
        :string-utils
        :list-utils
        :histogram
        :tensor)
  (:export :*gnuplot-session*
           :restart-gnuplot-session
           :titled
           :title
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
           :line-line-style
           :line-point-type
           :line-line-type
           :line-line-width
           :line-fill-style
           :line-fill-density
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
