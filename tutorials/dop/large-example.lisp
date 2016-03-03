;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013-2016 Gary Hollis
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


(require 'cl-ana)
(in-package :cl-ana)

(defproject large-example "/home/ghollisjr/dop-large-example/logged-results"
  (list #'macrotrans #'branchtrans #'tabletrans #'progresstrans)
  (fixed-cache 5))

(ensure-table-binding-ops)
(ensure-table-op-expanders)
(setf *print-progress* 10000)

(defun project-graph->png ()
  (dot->png (dot-compile "/home/ghollisjr/dop-large-example/graph.dot"
                         :if-exists :supersede)
            "/home/ghollisjr/dop-large-example/graph.dot"))

;;; Source data generation

;; Number of rows in the dataset
(defres (dataset nrows) 1000000)

;; Background contamination (background/(background + signal))
(defres background-ratio 0.5)
(defres (background nrows)
  (floor (* (res (dataset nrows))
            (res background-ratio))))
(defres (signal nrows)
  (- (res (dataset nrows))
     (res (background nrows))))

;; X sigmas
(defres (signal x sigma)
  1d0)
(defres (background x sigma)
  3d0)

;; Y slopes and offsets
(defres (signal slope)
  0.2)
(defres (signal offset mean)
  5d0)
(defres (signal offset sigma)
  0.5d0)

(defres (background slope)
  -0.2)
(defres (background offset mean)
  0d0)
(defres (background offset sigma)
  1d0)

;; Source data bootstrap
(defres bootstrap
  (with-open-hdf-file (file (work-path "data.h5")
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
    (let ((tab (create-hdf-table file
                                 "/table"
                                 '(("X" . :double)
                                   ("Y" . :double)))))
      ;; Signal
      (loop
         for i below (res (signal nrows))
         do
           (let* ((x (* (res (signal x sigma))
                        (alexandria:gaussian-random -5d0 5d0)))
                  (b (+ (* (res (signal offset sigma))
                           (alexandria:gaussian-random -5d0 5d0))
                        (res (signal offset mean))))
                  (y (+ (* (res (signal slope)) x)
                        b)))
             (table-push-fields tab
               x
               y)))
      ;; Background
      (loop
         for i below (res (background nrows))
         do
           (let* ((x (* (res (background x sigma))
                        (alexandria:gaussian-random -5d0 5d0)))
                  (b (+ (* (res (background offset sigma))
                           (alexandria:gaussian-random -5d0 5d0))
                        (res (background offset mean))))
                  (y (+ (* (res (background slope)) x)
                        b)))
             (table-push-fields tab
               x
               y)))
      (table-close tab))))

;;; Analyzing data

;; Source data
(defres src
  (srctab (hdf-opener (work-path "data.h5")
                      '(("X" . :double)
                        ("Y" . :double)))
          (res bootstrap)))

;; contaminated X histogram
(defres (src x hist)
  (dotab (res src)
      ((hist (make-shist '((:name "X" :low -9d0 :high 9d0 :nbins 100)))))
      hist
    (hins hist (list (field x)))))

(defres (plot (src x hist))
  (draw
   (page (list
          (plot2d (list
                   (line (res (src x hist))
                         :style "boxes"
                         :color "red"
                         :title "Contaminated X Distribution"))
                  :x-title "X"
                  :y-title "Count"))
         :output (work-path "plots/src/x-hist.jpg")
         :terminal (jpeg-term))))
