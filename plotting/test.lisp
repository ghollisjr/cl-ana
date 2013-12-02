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

;; Quick example of how to draw a single plot with a function and some
;; data:
(defparameter *quick-page*
  (quick-multidraw (list (list (zip (list 1 1.5 2 2.5 3)
                                    (list 0.9 1.2 1 0.3 0.1))
                               :title "test data"
                               :point-type 3)
                         (list "sin(x)" :title "sine"))
                   :page-title "Test plot"
                   :plot-title "Test plot"))

(defparameter *hist2d*
  (make-contiguous-hist
   (list (list :name "x" :nbins 100 :low -3d0 :high 3d0)
         (list :name "y" :nbins 100 :low -3d0 :high 3d0))))

(defun gaus () (alexandria:gaussian-random -3d0 3d0))

(loop for i below 100000 do (hist-insert *hist2d* (list (gaus) (gaus))))

(quick-draw *hist2d*)

(defparameter *hist1d*
  (hist-project *hist2d* "x")) 

;;(quick-draw *hist1d* :title "histogram" :color "black")

(defparameter *fit-results*
  (multiple-value-list
   (fit *hist1d*
        #'gaussian
        (list 1500d0 1d0 1d0)
        :max-iterations 25
        :prec 1e-8
        :derivative-delta 1e-12)))

(defparameter *fitfunc* (first *fit-results*))

(quick-multidraw (list (list *hist1d*
                             :title "Data Histogram"
                             :fill-style "solid"
                             :fill-density 0.3)
                       (list *fitfunc*
                             :title "Fitted Gaussian"
                             :color "blue"))
                 :x-title "x"
                 :y-title "N")
