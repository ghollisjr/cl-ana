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
