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
            "/home/ghollisjr/dop-large-example/graph.png"))

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
  (srctab (hdf-chain-opener (list (work-path "data.h5")))
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
                         :fill-style "solid"
                         :fill-density 0.3
                         :title "Contaminated X Distribution"))
                  :x-title "X"
                  :y-title "Count"))
         :output (work-path "plots/src/x-hist.png")
         :terminal (png-term))))

;; Model fitting:

(defun model (params x)
  (destructuring-bind (sigA sigMu sigSigma bacA bacMu bacSigma)
      params
    (+ (gaussian (list sigA sigMu sigSigma) x)
       (gaussian (list bacA bacMu bacSigma) x))))

(defres (model fit-results)
  (let* ((hist (res (src x hist)))
         (peak (maximum (cdrs (map->alist hist)))))
    (rest
     (multiple-value-list
      (fit (res (src x hist))
           #'model
           (list
            ;; Signal guess parameters
            (gauss-amp (* 0.5 peak)
                       1d0)
            0d0
            1d0
            ;; Background guess parameters
            (gauss-amp (* 0.5 peak)
                       1d0)
            0d0
            3d0))))))
(defres (model fit-params)
  (first (res (model fit-results))))
(defres (model fit)
  (let ((params (res (model fit-params))))
    (lambda (x)
      (model params x))))

;; Signal fit
(defres (model signal fit-params)
  (subseq (res (model fit-params))
          0 3))
(defres (model signal fit)
  (let ((params (res (model signal fit-params))))
    (lambda (x)
      (gaussian params x))))

;; Background fit
(defres (model background fit-params)
  (subseq (res (model fit-params))
          3))
(defres (model background fit)
  (let ((params (res (model background fit-params))))
    (lambda (x)
      (gaussian params x))))

;; Model plot:

(defres (plot (model signal background))
  (let* ((hist (res (src x hist)))
         (hist-alist (map->alist hist))
         (xmin (minimum (cars hist-alist)))
         (xmax (maximum (cars hist-alist)))
         (ymax (maximum (cdrs hist-alist)))
         (model (res (model fit)))
         (signal (res (model signal fit)))
         (signal-params (res (model signal fit-params)))
         (background (res (model background fit)))
         (background-params (res (model background fit-params))))
    (draw
     (page (list
            (plot2d (list
                     (line hist
                           :style "boxes"
                           :fill-style "solid"
                           :fill-density 0.3
                           :color "red"
                           :title "Contaminated X Distribution")
                     (line model
                           :sampling (list :nsamples 1000
                                           :low xmin
                                           :high xmax)
                           :style "lines"
                           :color "black"
                           :line-width 2
                           :title "Model Signal+Background")
                     (line signal
                           :sampling (list :nsamples 1000
                                           :low xmin
                                           :high xmax)
                           :style "lines"
                           :color "blue"
                           :line-width 2
                           :title (format nil
                                          "Signal (A=~,2e,mu=~,2e,sigma=~,2e)"
                                          (first signal-params)
                                          (second signal-params)
                                          (third signal-params)))
                     (line background
                           :sampling (list :nsamples 1000
                                           :low xmin
                                           :high xmax)
                           :style "lines"
                           :color "green"
                           :line-width 2
                           :title (format nil
                                          "Background (A=~,2e,mu=~,2e,sigma=~,2e)"
                                          (first background-params)
                                          (second background-params)
                                          (third background-params))))
                    :x-title "X"
                    :y-title "Count"
                    :y-range (cons 0 (* 1.2 ymax))))
           :output (work-path "plots/model.png")
           :terminal (png-term)))))

;;; Cut on Y

(defres (src x-y hist)
  (dotab (res src)
      ((hist (make-shist '((:name "X" :low -9d0 :high 9d0 :nbins 100)
                           (:name "Y" :low -5d0 :high 8d0 :nbins 100)))))
      hist
    (hins hist (list (field x)
                     (field y)))))

;; plot
(defres (plot (src x-y hist))
  (let* ((hist (res (src x-y hist))))
    (draw
     (page (list
            (plot2d (list
                     (line hist))
                    :title "X-Y Distribution"
                    :x-title "X"
                    :y-title "Y"))
           :output (work-path "plots/src/x-y-hist.jpg")
           :terminal (jpeg-term)))))

;;;; TODO:
;;;;
;;;; * Use branching to experiment with different X-Y cuts to isolate signal
;;;;
;;;;   * Includes branching on ltabs and a final selection
;;;;
;;;; * Plot different cuts all together in cut plot

(defres (y-cut slices)
  (hslice (res (src x-y hist))
          "X"))

(defres (y-cut x-range)
  (cons -2 2))

(defres (y-cut y-range)
  (cons 3 8))

(defres (y-cut slices filtered)
  (let* ((slices (res (y-cut slices)))
         (x-range (res (y-cut x-range)))
         (y-range (res (y-cut y-range)))
         (result (make-hash-table :test 'equal)))
    (loop
       for key being the hash-keys in slices
       for h being the hash-values in slices
       when (<= (car x-range)
                (first key)
                (cdr x-range))
       do (setf (gethash key result)
                (hist-filter (lambda (count &key y)
                               (<= (car y-range)
                                   y
                                   (cdr y-range)))
                             h)))
    result))

(defres (y-cut fit-results)
  (let* ((filtered (res (y-cut slices filtered)))
         (result (make-hash-table :test 'equal)))
    (loop
       for key being the hash-keys in filtered
       for h being the hash-values in filtered
       do
         (let* ((alist (map->alist h))
                (peak (maximum alist :key #'car))
                (xpeak (car peak))
                (ypeak (cdr peak)))
           (setf (gethash key result)
                 (rest
                  (multiple-value-list
                   (fit h
                        #'gaussian
                        (list (gauss-amp ypeak 0.5d0)
                              xpeak
                              0.5d0)))))))
    result))

(defres (y-cut fit-params)
  (let* ((fit-results (res (y-cut fit-results)))
         (result (make-hash-table :test 'equal)))
    (loop
       for key being the hash-keys in fit-results
       for fr being the hash-values in fit-results
       do (setf (gethash key result)
                (first fr)))
    result))

(defres (y-cut fits)
  (let* ((fit-params (res (y-cut fit-params)))
         (result (make-hash-table :test 'equal)))
    (loop
       for key being the hash-keys in fit-params
       for fp being the hash-values in fit-params
       do (setf (gethash key result)
                (let ((pars (copy-list fp)))
                  (lambda (x)
                    (gaussian pars x)))))
    result))
(logres-ignore (y-cut fits))

;; Plot of slice fits:
(defres (plot (y-cut slice fits))
  (let* ((filtered (res (y-cut slices filtered)))
         (fits (res (y-cut fits)))
         (fit-params (res (y-cut fit-params))))
    (loop
       for key being the hash-keys in filtered
       for h being the hash-values in filtered
       do (let* ((x (first key))
                 (alist (map->alist h))
                 (xmin (minimum (cars alist)))
                 (xmax (maximum (cars alist)))
                 (ymax (maximum (cdrs alist)))
                 (fit (gethash key fits))
                 (params (gethash key fit-params))
                 (A (first params))
                 (mu (second params))
                 (sigma (third params)))
            (draw
             (page (list
                    (plot2d (list
                             (line h
                                   :title "Y Distribution"
                                   :color "red"
                                   :style "boxes"
                                   :fill-style "solid"
                                   :fill-density 0.3)
                             (line fit
                                   :title
                                   (format nil
                                           "Gaussian Fit (A=~,2e,mu=~,2e,sigma=~,2e)"
                                           A mu sigma)
                                   :sampling (list :nsamples 1000
                                                   :low xmin
                                                   :high xmax)
                                   :color "black"
                                   :style "lines"
                                   :line-width 2))
                            :title (format nil "Y Slice Fit for X=~,2e"
                                           x)
                            :x-title "Y"
                            :x-range (cons xmin xmax)
                            :y-title "count"
                            :y-range (cons 0 (* 1.2 ymax))))
                   :output (work-path "plots/y-cut/slices/X_~,4f.jpg" x)
                   :terminal (jpeg-term)))))))

;;; Branching on nsigma

(defres (branching y-cut-nsigmas)
  (branch (range 1 3 1)))

(defres (branching y-cut lower-bounds)
  (branch (res (branching y-cut-nsigmas))
          (let ((fit-params (res (y-cut fit-params))))

            (sort (loop
                     for key being the hash-keys in fit-params
                     for fp being the hash-values in fit-params
                     collecting
                       (let ((mu (second fp))
                             (sigma (third fp)))
                         (cons (first key)
                               (- mu (* (branch)
                                        sigma)))))
                  #'<
                  :key #'car))))

(defres (branching y-cut upper-bounds)
  (branch (res (branching y-cut-nsigmas))
          (let ((fit-params (res (y-cut fit-params))))

            (sort (loop
                     for key being the hash-keys in fit-params
                     for fp being the hash-values in fit-params
                     collecting
                       (let ((mu (second fp))
                             (sigma (third fp)))
                         (cons (first key)
                               (+ mu (* (branch)
                                        sigma)))))
                  #'<
                  :key #'car))))

;; lower fit
(defres (branching y-cut lower-bounds fit-results)
  (branch (res (branching y-cut-nsigmas))
          (rest
           (multiple-value-list
            (fit (res (branching y-cut lower-bounds))
                 #'polynomial
                 (list 1d0 1d0))))))
(defres (branching y-cut lower-bounds fit-params)
  (branch (res (branching y-cut-nsigmas))
          (first (res (branching y-cut lower-bounds fit-results)))))
(defres (branching y-cut lower-bounds fit)
  (branch (res (branching y-cut-nsigmas))
          (let ((ps (res (branching y-cut lower-bounds fit-params))))
            (lambda (x)
              (polynomial ps x)))))
(logres-ignore (branching y-cut lower-bounds fit))

;; upper fit
(defres (branching y-cut upper-bounds fit-results)
  (branch (res (branching y-cut-nsigmas))
          (rest
           (multiple-value-list
            (fit (res (branching y-cut upper-bounds))
                 #'polynomial
                 (list 1d0 1d0))))))
(defres (branching y-cut upper-bounds fit-params)
  (branch (res (branching y-cut-nsigmas))
          (first (res (branching y-cut upper-bounds fit-results)))))
(defres (branching y-cut upper-bounds fit)
  (branch (res (branching y-cut-nsigmas))
          (let ((ps (res (branching y-cut upper-bounds fit-params))))
            (lambda (x)
              (polynomial ps x)))))
(logres-ignore (branching y-cut upper-bounds fit))

;; y-cut plot

(defres (plot (y-cut branching))
  (let* ((lower-bounds (res (branching y-cut lower-bounds)))
         (upper-bounds (res (branching y-cut upper-bounds)))
         (lower-fit (res (branching y-cut lower-bounds fit)))
         (upper-fit (res (branching y-cut upper-bounds fit)))
         (x-range (res (y-cut x-range)))
         (y-range (res (y-cut y-range)))
         (hist (res (src x-y hist)))
         (hist-alist (map->alist hist))
         (xs (cars (cars hist-alist)))
         (xmin (minimum xs))
         (xmax (maximum xs))
         (ys (cars (cdrs (cars hist-alist))))
         (ymin (minimum ys))
         (ymax (maximum ys)))
    (draw
     (page (list
            (plot2d (append
                     ;; Non-branching
                     (list
                      ;; histogram
                      (line hist)
                      ;; left edge
                      (line (list (cons (car x-range) (car y-range))
                                  (cons (car x-range) (cdr y-range)))
                            :title "Fit Area"
                            :color "black"
                            :style "lines"
                            :line-width 2)
                      ;; right edge
                      (line (list (cons (cdr x-range) (car y-range))
                                  (cons (cdr x-range) (cdr y-range)))
                            :title ""
                            :color "black"
                            :style "lines"
                            :line-width 2)
                      ;; top edge
                      (line (list (cons (car x-range) (cdr y-range))
                                  (cons (cdr x-range) (cdr y-range)))
                            :title ""
                            :color "black"
                            :style "lines"
                            :line-width 2)
                      ;; bottom edge
                      (line (list (cons (car x-range) (car y-range))
                                  (cons (cdr x-range) (car y-range)))
                            :title ""
                            :color "black"
                            :style "lines"
                            :line-width 2)
                      ;; Function legend entry
                      (line (list (cons -100 -100))
                            :title "Upper Cut Functions"
                            :color "red"
                            :style "lines"
                            :line-width 2)
                      (line (list (cons -100 -100))
                            :title "Lower Cut Functions"
                            :color "green"
                            :style "lines"
                            :line-width 2))
                     ;; Branching
                     (loop
                        for key being the hash-keys in lower-bounds
                        for lb being the hash-values in lower-bounds
                        for i from 1
                        appending
                          (let* ((ub (gethash key upper-bounds))
                                 (lowfit (gethash key lower-fit))
                                 (upfit (gethash key upper-fit)))
                            (list
                             ;; Upper bounds
                             (line ub
                                   :title (format nil
                                                  "Cut Bounds (nsigma=~,2f)"
                                                  key)
                                   :style "points"
                                   :point-type i
                                   :color "orange"
                                   :point-size 1)
                             ;; Lower bounds
                             (line lb
                                   :title ""
                                   :style "points"
                                   :point-type i
                                   :color "orange"
                                   :point-size 1)
                             ;; Upper fit
                             (line upfit
                                   :sampling (list :nsamples 1000
                                                   :low (car x-range)
                                                   :high (cdr x-range))
                                   :title ""
                                   :style "lines"
                                   :color "red"
                                   :line-width 2)
                             ;; Lower fit
                             (line lowfit
                                   :sampling (list :nsamples 1000
                                                   :low (car x-range)
                                                   :high (cdr x-range))
                                   :title ""
                                   :style "lines"
                                   :color "green"
                                   :line-width 2)))))
                    :legend (legend :location (cons :left :top))
                    :x-title "X"
                    :x-range (cons xmin xmax)
                    :y-title "Y"
                    :y-range (cons ymin (* 2 ymax))))
           :output (work-path "plots/y-cut/branching-cuts.jpg")
           :terminal (jpeg-term)))))

;; Cut function:
(defres (branching y-cut)
  (branch (res (branching y-cut-nsigmas))
          (let ((lower-fit (res (branching y-cut lower-bounds fit)))
                (upper-fit (res (branching y-cut upper-bounds fit))))
            (lambda (x y)
              (and (<= (car (res (y-cut x-range)))
                       x
                       (cdr (res (y-cut x-range))))
                   (<= (funcall lower-fit x)
                       y
                       (funcall upper-fit x)))))))
(logres-ignore (branching y-cut))

;; y-cut table
(defres (branching src y-cut)
  (branch (res (branching y-cut-nsigmas))
          (ltab (res src)
              ()
            (when (funcall (res (branching y-cut))
                           (field x)
                           (field y))
              (push-fields)))))

(defres (branching src y-cut x hist)
  (branch (res (branching y-cut-nsigmas))
          (dotab (res (branching src y-cut))
              ((hist (make-shist '((:name "X" :low -9d0 :high 9d0 :nbins 100)))))
              hist
            (hins hist (list (field x))))))

(defres (plot (y-cut x hist branching normalized))
  (let* ((hists (res (branching src y-cut x hist)))
         (x-range (res (y-cut x-range)))
         ;; Peak normalization for shape comparison
         (peaks
          (loop
             for nsigmas being the hash-keys in hists
             for h being the hash-values in hists
             collecting (let* ((alist (map->alist h))
                               (peak (maximum (cdrs alist))))
                          (cons nsigmas
                                peak))))
         (fact-alist (mapcar (lambda (cons)
                               (cons (car cons)
                                     (/ (cdr cons))))
                             peaks))
         (factors (map->hash-table fact-alist 'equal)))
    (draw
     (page (list
            (plot2d (append
                     (list
                      ;; X fit-range bounds
                      (line (list (cons (car x-range) 0)
                                  (cons (car x-range) 1e9))
                            :title "X fit-range"
                            :style "lines"
                            :color "black")
                      (line (list (cons (cdr x-range) 0)
                                  (cons (cdr x-range) 1e9))
                            :title ""
                            :style "lines"
                            :color "black"))
                     (loop
                        for nsigmas being the hash-keys in hists
                        for hist being the hash-values in hists
                        for point-type from 1
                        for color
                        in (list "blue" "green" "red"
                                 ;; some extras in case we try more values
                                 "orange" "purple" "yellow" "brown")
                        collecting
                          (line (* (gethash nsigmas factors)
                                   hist)
                                :color color
                                :point-type point-type
                                :point-size 2
                                :title (format nil "X Distribution (nsigma=~a)"
                                               nsigmas)
                                :style "points")))
                    :title "Comparison of Y-cut effects on X distribution"
                    :legend (legend :front-p t
                                    :location (cons :right :top)
                                    :box t
                                    :width-inc 0.5)
                    :x-title "X"
                    :x-range (cons "*" "*")
                    :y-title "Peak-Normalized Count"
                    :y-range (cons 0 1.3)))
           :output (work-path "plots/X-hist-y-cut-branching.jpg")
           :terminal (jpeg-term)))))

(defres y-cut-nsigmas
  3)

(defres y-cut
  (gethash (res y-cut-nsigmas)
           (res (branching y-cut))))
(logres-ignore y-cut)

;; y-cut table
(defres (src y-cut)
  (tab (res src)
      ()
      (hdf-opener (work-path "y-cut.h5")
                  (list (cons "X" :double)
                        (cons "Y" :double)))
    (when (funcall (res y-cut)
                   (field x)
                   (field y))
      (push-fields
       (x (field x))
       (y (field y))))))

(defres (src y-cut x hist)
  (gethash (res y-cut-nsigmas)
           (res (branching src y-cut x hist))))

(defres (src y-cut x-y hist)
  (dotab (res (src y-cut))
      ((x-range (res (y-cut x-range)))
       (y-range (res (y-cut y-range)))
       (hist (make-shist
              (list (list :name "X"
                          :low (car x-range)
                          :high (cdr x-range)
                          :nbins 100)
                    (list :name "Y"
                          :low (car y-range)
                          :high (cdr y-range)
                          :nbins 100)))))
      hist
    (hins hist (list (field x)
                     (field y)))))
