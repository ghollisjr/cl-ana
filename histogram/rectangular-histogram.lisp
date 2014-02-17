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
;;;; rectangular-histogram.lisp

;;;; This probably needs a lot of work to make it logically clear
;;;; what's going on, but at the moment it does work and I don't feel
;;;; like breaking it while I try to do get plotting done.
;;;;
;;;; For example there is currently code duplication for generating
;;;; the bin-specs from the plists in the contiguous and sparse
;;;; histograms, so this should be placed in this histogram class.

(in-package :histogram)

;;;; Provides a rectangular histogram; i.e. the binning is fixed-width
;;;; and the width is uniform in every dimension.
(defclass rectangular-histogram (histogram)
  ((bin-specs
    :accessor rectangular-hist-bin-specs
    :initarg :bin-specs
    :initform nil
    :documentation "List of number of bins, bin low edge, and bin high
    edge.  Should be computed from a bin specification plist and then
    stored in said order as a list (not a plist).")))

(defmethod hist-slice ((hist rectangular-histogram) &rest dims)
  (let* ((empty-bin-value
          (hist-empty-bin-value hist))
         (default-increment
          (hist-default-increment hist))
         (type-constructor
          (type-constructor hist))
         (all-dim-specs
          (hist-dim-specs hist))
         (all-dims
          (hdn hist))
         (slice-indices (get-dim-indices all-dims dims))
         (remaining-dim-specs
          (except-at all-dim-specs slice-indices))
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
                       remaining-dim-specs
                       :empty-bin-value empty-bin-value
                       :default-increment default-increment))
              (setf (gethash slice-centers result)
                    h))
            (hins h other-centers count)))
    result))

(defun get-bin-index (data-list bin-specs)
  "Computes the bin-index-list from the data-list (a data point).
Does so for all axes, returns nil if overflow/underflow occurs.

Make sure that data-list and bin-specs have the same length before
calling this function; it is unchecked in this respect for speed."
  (labels
      ((get-bin-index-worker (data-list bin-specs &optional result)
	 (if data-list ; I'm intentionally ignoring length differences
		       ; between data-list and bin-specs
	     (let* ((datum (first data-list))
		    (bin-spec (first bin-specs))
		    (bin-index (get-axis-bin-index datum bin-spec)))
	       (if (not (or (equal bin-index :underflow)
			    (equal bin-index :overflow)))
		   (get-bin-index-worker (rest data-list)
					 (rest bin-specs)
					 (cons bin-index result))))
	     (nreverse result))))
    (get-bin-index-worker data-list bin-specs)))

(defun get-axis-bin-index (value bin-spec)
  "Computes the bin index (-1 underflow, binnum overflow) for value
given the bin-spec for a single axis/dimension."
  (let* ((nbins (first bin-spec))
	 (binlo (second bin-spec))
	 (binhi (third bin-spec))
	 (delta (/ (- binhi binlo) nbins)))
    (labels ((internal-bin (i)
	       (cond ((< i 0) :underflow)
		     ((> i (- nbins 1)) :overflow)
		     (t i))))
      (internal-bin
       (floor
	(/ (- value binlo) delta))))))

(defun get-bin-center-worker (bin-spec index)
  (let* ((nbins (first bin-spec))
	 (binlo (second bin-spec))
	 (binhi (third bin-spec))
	 (delta (/ (- binhi binlo) nbins)))
    (+ binlo
       (/ delta 2)
       (* delta
	  index))))

(defun get-bin-center (bin-specs bin-index)
  (mapcar #'get-bin-center-worker bin-specs bin-index))

(defun hist-dim-specs (histogram)
  "Returns the plists denoting the name, nbins, low, and high for each
dimension in the histogram."
  (loop
     for bin-spec in (rectangular-hist-bin-specs histogram)
     for dim-name in (hist-dim-names histogram)
     collecting (destructuring-bind (nbins low high)
                    bin-spec
                  (list :name dim-name
                        :nbins nbins
                        :low low
                        :high high))))

;; abbrevations:

(defun hds (histogram)
  "Abbreviation for hist-dim-specs"
  (hist-dim-specs histogram))
