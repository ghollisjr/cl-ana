;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013, 2014 Gary Hollis
;;;; 
;;;; This file is part of cl-ana.
;;;; 
;;;; cl-ana is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; cl-ana is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with cl-ana.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis (me!) via email at
;;;; ghollisjr@gmail.com
;;;; package.lisp

(defpackage #:plotting 
  (:use :cl
        :math-functions
        :error-propogation
        :gnuplot-interface
	:map
        :string-utils
        :list-utils
        :macro-utils
        :histogram
        :tensor)
  (:export :*gnuplot-sessions*
           :*gnuplot-single-session*
           :restart-gnuplot-sessions
           :titled
           :title
           :page
	   :plot
	   :plot2d
	   :plot3d
	   :line
           :data-line
           :analytic-line
           :legend
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
           :make-line
           ;; utilities
           :sample-function))

(gmath:use-gmath :plotting)
