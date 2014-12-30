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

(in-package :cl-ana.histogram)

;; I've realized that histograms are an instance of a more general
;; idea: Categorization/Grouping.  The categories are the bin ranges,
;; and can be represented by the bin centers as long as all bin
;; properties are known from this (it seems obvious but it took a
;; while to realize).
;;
;; Also: sparse and contiguous histograms are provided for efficiency
;; reasons only, since categorical-histograms are general enough for
;; any histogramming, including nonuniform-binning

(defclass categorical-histogram (histogram)
  ((bin-table
    :initarg :bin-table
    :initform (make-hash-table :test 'equal)
    :accessor cathist-bin-table)))

(defun make-categorical-hist
    (dim-names
     &key
       default-increment
       empty-bin-value)
  #k(make-instance 'categorical-histogram
                   :dim-names dim-names
                   :ndims (length dim-names)
                   &when-keys
                   default-increment
                   empty-bin-value))

(defun make-cathist (&rest args)
  "Short form of make-categorical-hist"
  (apply #'make-categorical-hist args))

(defmethod type-constructor ((hist categorical-histogram))
  #'make-cathist)

(defmethod empty-copy ((hist categorical-histogram))
  (with-slots (dim-names default-increment empty-bin-value)
      hist
    (make-cathist dim-names
                  :default-increment default-increment
                  :empty-bin-value empty-bin-value)))

(defmethod hist-insert ((hist categorical-histogram)
                        datum &optional weight)
  (with-slots (bin-table empty-bin-value default-increment)
      hist
    (let ((weight (if weight weight default-increment)))
      (if (gethash datum bin-table)
          (incf (gethash datum bin-table)
                weight)
          (setf (gethash datum bin-table)
                (+ empty-bin-value weight)))))
  hist)

(defmethod hist-point-ref ((hist categorical-histogram)
                           point)
  (with-slots (bin-table)
      hist
    (aif (gethash point bin-table)
         it
         (hist-empty-bin-value hist))))

(defmethod hist-bin-values ((hist categorical-histogram))
  (with-slots (bin-table)
      hist
    (loop
       for k being the hash-keys in bin-table
       for v being the hash-values in bin-table
       collecting (cons v k))))

(defmethod hist-total-integral ((hist categorical-histogram))
  (sum (mapcar #'car
               (hist-bin-values hist))))

(defmethod hist-integrate ((hist categorical-histogram) &rest axes)
  (with-slots (ndims
               dim-names
               empty-bin-value
               default-increment)
      hist
    (let* ((indices
            (get-dim-indices dim-names axes))
           (remaining-dim-names
            (except-at dim-names indices))
           (remaining-indices
            (except-at (range 0 (1- ndims)) indices))
           (result
            (make-cathist remaining-dim-names
                          :empty-bin-value empty-bin-value
                          :default-increment default-increment)))
      (loop
         for (count . centers) in (hist-bin-values hist)
         do (hist-insert result
                         (at-indices centers remaining-indices)
                         count))
      result)))

;; for hist-project
(defmethod hist-reorder-dimensions ((hist categorical-histogram)
                                    dim-indices)
  (let ((result (empty-copy hist)))
    (loop
       for (count . centers) in (hist-bin-values hist)
       do (hist-insert result
                       (permute centers dim-indices)
                       count))
    result))
         
(defmethod hist-slice ((hist categorical-histogram) &rest dims)
  (let* ((empty-bin-value
          (hist-empty-bin-value hist))
         (default-increment
          (hist-default-increment hist))
         (type-constructor
          (type-constructor hist))
         (all-dim-names
          (hist-dim-names hist))
         (all-dims
          (hdn hist))
         (slice-indices (get-dim-indices all-dims dims))
         (remaining-dim-names
          (except-at all-dim-names slice-indices))
         (remaining-indices
          (except-at (range 0 (1- (hist-ndims hist)))
                     slice-indices))
         (bin-values (hbv hist))
         (result (make-hash-table :test 'equal)))
    (loop
       for (count . centers) in bin-values
       do (let* ((slice-centers
                  (at-indices centers slice-indices))
                 (other-centers
                  (at-indices centers remaining-indices))
                 (h (gethash slice-centers result)))
            (when (not h)
              (setf h (funcall
                       type-constructor
                       remaining-dim-names
                       :empty-bin-value empty-bin-value
                       :default-increment default-increment))
              (setf (gethash slice-centers result)
                    h))
            (hins h other-centers count)))
    result))
