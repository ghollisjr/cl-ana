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

;;;; Provides common functions for contiguous-histogram and
;;;; sparse-histogram due to uniform binning

(in-package :cl-ana.histogram)
 
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
