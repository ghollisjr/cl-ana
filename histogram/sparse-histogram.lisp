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

;;;; Implements sparse histogram with uniform binning

(in-package :cl-ana.histogram)

(defclass sparse-histogram (rectangular-histogram)
  ((bin-value-map
    :accessor sparse-hist-value-map
    :initarg :value-map
    :initform nil
    :documentation "Hash table storing the histogram bin values which
    have been set.")))

(defun make-sparse-hist (dim-spec-plists &key
					   empty-bin-value
					   default-increment
					   hash-table-test)
  "dim-spec-plist is a plist with the following
fields: :name :low :high :nbins, which specify the axis name,
bin-low-edge, bin-high-edge, and the number of bins on the axis
respectively.

Example usage: (make-sparse-hist (list (list :name \"x\" :nbins
10 :low 50 :high 55))) would create a sparse histogram with one
dimension named \"x\" with 10 bins, low bin edge 50 and high bin edge
55."
  (let* ((dim-names (mapcar (lambda (spec) (getf spec :name))
			    dim-spec-plists))
	 (ndims (length dim-names))
	 (bin-specs (mapcar (lambda (s)
                              (list (getf s :nbins)
                                    (getf s :low)
                                    (getf s :high)))
			    dim-spec-plists))
	 (default-args (progn
			 (let ((result ()))
			   (when empty-bin-value
			     (push empty-bin-value result)
			     (push :empty-bin-value result))
			   (when default-increment
			     (push default-increment result)
			     (push :default-increment result))
			   result))))
    (apply #'make-instance 'sparse-histogram
	   :ndims ndims
	   :dim-names dim-names
	   :bin-specs bin-specs
	   :value-map (make-hash-table
		       :test (if hash-table-test
				 hash-table-test
				 'equal))
	   default-args)))

(defun make-shist (dim-spec-plists &key empty-bin-value default-increment)
  "Abbreviation for make-sparse-hist"
  #k(make-sparse-hist dim-spec-plists
                      &when-keys
                      empty-bin-value
                      default-increment))

(defmethod empty-copy ((h sparse-histogram))
  (with-slots (empty-bin-value
               default-increment)
      h
    (make-shist (hist-dim-specs h)
                :empty-bin-value empty-bin-value
                :default-increment default-increment)))

;; For now I'll use this; since I'm not taking into account empty bins
;; during the other calculations I guess I'm not going to do it here
;; either--emphasis on for now.
(defmethod hist-total-integral ((hist sparse-histogram))
  (let ((bin-values (hist-bin-values hist)))
    (if bin-values
        (sum (mapcar #'car bin-values))
        0)))

(defmethod hist-integrate ((histogram sparse-histogram) &rest axes)
  (if axes
      (with-accessors ((ndims hist-ndims)
                       (dim-names hist-dim-names)
                       (empty-bin-value hist-empty-bin-value)
                       (default-increment hist-default-increment)
                       (value-map sparse-hist-value-map)
                       (bin-specs rectangular-hist-bin-specs))
          histogram
        (if (length-equal axes ndims)
            (hist-total-integral histogram)
            (flet ((index-key (x)
                     (if (listp x)
                         (first x)
                         x)))
              (let* ((axis-name-or-indices (mapcar (lambda (x)
                                                     (if (listp x)
                                                         (first x)
                                                         x))
                                                   axes))
                     (dim-indices (get-dim-indices dim-names axis-name-or-indices))
                     (point-bounds
                      (mapcar (lambda (x)
                                (when (listp x)
                                  (rest x)))
                              axes))
                     (index-bounds
                      (mapcar (lambda (point-bound bin-spec)
                                (when point-bound
                                  (let ((low-index
                                         (get-axis-bin-index (first point-bound)
                                                             bin-spec))
                                        (high-index
                                         (get-axis-bin-index (second point-bound)
                                                             bin-spec)))
                                    (list (if (equal low-index :underflow)
                                              0
                                              low-index)
                                          (if (equal high-index :overflow)
                                              (1- (first bin-spec))
                                              high-index)))))
                              point-bounds
                              bin-specs))
                     (index-specs (mapcar (lambda (x y)
                                            (if y
                                                (cons x y)
                                                x))
                                          dim-indices
                                          index-bounds))
                     (index-specs-copy (copy-list index-specs))
                     (sorted-index-specs (sort index-specs-copy #'>
                                               :key #'index-key))
                     (unique-sorted-index-specs
                      (reduce
                       (lambda (x y) (adjoin y x
                                             :test #'equal
                                             :key #'index-key))
                       sorted-index-specs
                       :initial-value ()))
                     (unique-sorted-indices
                      (mapcar (lambda (x)
                                (if (listp x)
                                    (first x)
                                    x))
                              unique-sorted-index-specs))
                     (new-ndims (cl:- ndims (length unique-sorted-indices)))
                     (new-dim-names (except-at dim-names unique-sorted-indices
                                               :uniquely-sorted t))
                     (new-bin-specs (except-at bin-specs unique-sorted-indices
                                               :uniquely-sorted t)))
                (make-instance 'sparse-histogram
                               :ndims new-ndims
                               :dim-names new-dim-names
                               :empty-bin-value empty-bin-value
                               :default-increment default-increment
                               :value-map (sparse-hist-integrate-contents
                                           value-map
                                           index-specs
                                           ;;unique-sorted-index-specs
                                           empty-bin-value)
                               :bin-specs new-bin-specs)))))
      histogram))

(defmethod hist-insert ((hist sparse-histogram) datum &optional weight)
  (with-accessors ((value-map sparse-hist-value-map)
		   (empty-bin-value hist-empty-bin-value))
      hist
    (let ((point (mklist datum))
          (weight-factor (if weight
			     weight
			     (hist-default-increment hist))))
      (cond-setf (hist-point-ref hist point)
		 (+ (hist-point-ref hist point)
		    weight-factor)
		 :place)
      hist)))

(defmethod hist-index-ref ((hist sparse-histogram) index-list)
  "Unchecked, assumes you know what the allowed index values are."
  (with-accessors ((value-map sparse-hist-value-map)
		   (empty-bin-value hist-empty-bin-value))
      hist
    (gethash index-list value-map empty-bin-value)))

(defmethod (setf hist-index-ref) (value (hist sparse-histogram) index-list)
  "Unchecked, assumes you know what the allowed index values are."
  (with-accessors ((value-map sparse-hist-value-map))
      hist
    (setf (gethash index-list value-map) value)))

(defmethod hist-point-ref ((hist sparse-histogram) point)
  "Checked access to the bin value via a point.  Returns nil if the
point is not inside the histogram domain."
  (with-accessors ((bin-specs rectangular-hist-bin-specs)
		   (value-map sparse-hist-value-map)
		   (empty-bin-value hist-empty-bin-value))
      hist
    (let ((bin-index (get-bin-index point bin-specs)))
      (when bin-index
	(gethash bin-index value-map empty-bin-value)))))

(defmethod (setf hist-point-ref) (value (hist sparse-histogram) point)
  "Checked setf to the bin value via a point.  Does nothing & returns
nil if the point is not inside the histogram domain."
  (with-accessors ((bin-specs rectangular-hist-bin-specs)
		   (value-map sparse-hist-value-map))
      hist
    (let ((bin-index (get-bin-index point bin-specs)))
      (when bin-index
	(setf (gethash bin-index value-map) value)))))

(defmethod hist-bin-values ((hist sparse-histogram))
  (with-accessors ((value-map sparse-hist-value-map)
		   (bin-specs rectangular-hist-bin-specs))
      hist
    (iter:iter
      (iter:for (key val) in-hashtable value-map)
      (iter:collect (cons val (get-bin-center bin-specs key))))))

;;; Generic Math functions:
;; It may not be in the best style to implement mathematical functions
;; on sparse histograms.  Reason: They use un-ordered containers (hash
;; tables) for the data.  That means that, unless assumptions are made
;; about the default value, it will be extremely inefficient to try to
;; do computations using the sparse histograms.  It would be better to
;; do computations with contiguous histograms by first integrating the
;; sparse histogram into a size small enough to be operated on.  But I
;; may change my mind and implement the basic functions anyways.

;;; Internal use functions

(defun sparse-hist-integrate-contents (value-map index-specs empty-bin-value)
  "Integrates over the bin value hash table according to the
index-specs."
  (reduce (rcurry #'sparse-hist-integrate-contents-worker empty-bin-value)
	  index-specs
	  :initial-value value-map))

(defun sparse-hist-integrate-contents-worker (value-map index-spec empty-bin-value)
  "Integrates the bin value hash table along the index in index-spec;
if index-spec is a list, then the axis index is taken from the first
value, and then the index low/high cutoffs are taken from the second
and third elements respectively."
  (let* ((test (hash-table-test value-map))
	 (result (make-hash-table :test test)))
    (if (listp index-spec)
	;; handle case with bounds
	(let ((axis (first index-spec))
	      (low-bound (second index-spec))
	      (high-bound (third index-spec)))
	  (iter:iter
            (iter:for (key val) in-hashtable value-map)
            (let ((this-axis (elt key axis)))
              (when (and (<= low-bound this-axis)
                         (< this-axis high-bound))
                (setf (gethash (except-nth key axis) result)
                      (cl:+ (gethash (except-nth key axis) result empty-bin-value)
                            val))))))
	;; handle the unbounded case
	(let ((axis index-spec))
	  (iter:iter
            (iter:for (key val) in-hashtable value-map)
            (setf (gethash (except-nth key axis) result)
                  (cl:+ (gethash (except-nth key axis) result empty-bin-value)
                        val)))))
    result))

(defmethod hist-reorder-dimensions ((histogram sparse-histogram) dim-indices)
  (if (equal dim-indices (range 0 (length dim-indices)))
      histogram
      (let ((result
             (make-sparse-hist (permute (hist-dim-specs histogram)
                                        dim-indices)
                               :empty-bin-value
                               (hist-empty-bin-value histogram)
                               :default-increment
                               (hist-default-increment histogram))))
        (loop
           for bin-value in (hist-bin-values histogram)
           do (hist-insert result
                           (permute (rest bin-value)
                                    dim-indices)
                           (first bin-value)))
        result)))

(defmethod type-constructor ((hist sparse-histogram))
  #'make-sparse-hist)
