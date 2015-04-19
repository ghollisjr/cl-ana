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

(in-package :cl-ana.histogram)

(declaim (optimize (speed 3)))

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
                    (nbins (the fixnum (first bin-spec)))
		    (bin-index (the fixnum
                                    (get-axis-bin-index datum bin-spec))))
	       (when (not (or (minusp bin-index)
                              (>= bin-index nbins)))
                 (get-bin-index-worker (rest data-list)
                                       (rest bin-specs)
                                       (cons bin-index result))))
	     (nreverse result))))
    (get-bin-index-worker data-list bin-specs)))

(defun get-axis-bin-index (value bin-spec)
  "Computes the bin index (-1 underflow, binnum overflow) for value
given the bin-spec for a single axis/dimension."
  (declare (list bin-spec)
           (real value))
  (destructuring-bind (nbins binlo binhi) bin-spec
    (declare (fixnum nbins))
    (let ((delta (cl:/ (cl:- binhi binlo) nbins)))
      (the fixnum (floor (cl:- value binlo) delta)))))

(defun get-bin-center-worker (bin-spec index)
  (let* ((nbins (first bin-spec))
	 (binlo (second bin-spec))
	 (binhi (third bin-spec))
	 (delta (cl:/ (cl:- binhi binlo) nbins)))
    (declare (fixnum nbins))
    (cl:+ binlo
          (cl:/ delta 2)
          (cl:* delta
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

(defun hist-bin-widths (hist)
  (mapcar (lambda (lst)
            (destructuring-bind (&key low high nbins &allow-other-keys)
                lst
              (cl:/ (cl:- high low) nbins)))
          (hist-dim-specs hist)))

;; abbrevations:

(defun hds (histogram)
  "Abbreviation for hist-dim-specs"
  (hist-dim-specs histogram))

(defun hbw (histogram)
  (hist-bin-widths histogram))

;; Ease of use:

;; nbins * delta + low = high
;; -> delta = (high - low)/nbins
;; if nbins = floor((high-low/delta)) then
;; newhigh = floor((high-low)/delta)*delta + low
(defun discrete-dim-spec (&key low high (delta 1d0))
  "Returns dim-spec for discrete/integral data, e.g. digital event
counter readouts."
  (let* ((newlow (cl:- low
                       (cl:* delta 0.5)))
         (newnbins (1+ (floor (cl:- high low)
                              delta)))
         (newhigh (cl:+ low
                        (cl:* (1- newnbins)
                              delta)
                        (cl:* delta 0.5))))
    (list :low newlow
          :high newhigh
          :nbins newnbins)))

(defun dds (&rest args)
  "Abbreviation for discrete-dim-spec"
  (apply #'discrete-dim-spec args))
