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
(require 'plotting)

(require 'fitting)

(require 'alexandria)

(in-package :plotting)

(use-package :fitting)

;; Long example of how to use raw types for structure:
(defparameter *page*
  (make-instance
   'page
   :title "Just a couple plots"
   :layout (cons 1 2)
   :dimensions (cons 1300 600)
   :scale (cons 1 1)
   :plots
   (list (make-instance
          'plot2d
          :title "Plot 1"
          :x-range (cons -1 1)
          :y-range (cons -2 2)
          :lines
          (list (make-instance
                 'analytic-line
                 :title "Sine function"
                 :fn-string "sin(x)"
                 :color "blue")
                (make-instance
                 'analytic-line
                 :title "Cosine function"
                 :fn-string "cos(x)")))
         (make-instance
          'plot2d
          :title "Plot 2"
          :lines
          (list (make-instance
                 'data-line
                 :title "Some data"
                 :style "points"
                 :data (list (cons 1 1)
                             (cons 2 2)
                             (cons 3 3)
                             (cons 4 4))))))))

(draw *page*)

;; Quick example of how to draw a single plot with a function and some
;; data:
(defparameter *quick-page*
  (draw (lines (list (zip (list 1 1.5 2 2.5 3)
                          (list 0.9 1.2 1 0.3 0.1))
                     :title "test data"
                     :point-type 3)
               (list "sin(x)" :title "sine"))
        :page-args '(:title "Test plot")
        :plot-args '(:title "Test plot")))

(defparameter *hist2d*
  (make-chist
   (list (list :name "x" :nbins 100 :low -3d0 :high 3d0)
         (list :name "y" :nbins 100 :low -3d0 :high 3d0))))

(defun gaus ()
  (alexandria:gaussian-random -3d0 3d0))

(loop
   for i below 100000
   do (hist-insert *hist2d*
		   (list (gaus) (gaus))))

(draw *hist2d*)

(defparameter *hist1d*
  (hist-project *hist2d* "x"))

(defparameter *fit-results*
  (multiple-value-list
   (fit *hist1d*
        #'gaussian
        (list 1500d0 1d0 1d0)
        :max-iterations 25
        :prec 1e-8
        :derivative-delta 1e-12)))

(defparameter *fitfunc* (first *fit-results*))

(draw
 (list (list *hist1d*
             :title "Data Histogram"
             :fill-style "empty"
             ;;:fill-density 0.1
             :color "black")
       (list *fitfunc*
             :title "Fitted Gaussian"
             :color "red"))
 :plot-args '(:x-title "x"
              :y-title "N"))
