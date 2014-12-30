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

;;;; Implements histograms with binning which is both contiguous and
;;;; uniform.

(in-package :cl-ana.histogram)

(defclass contiguous-histogram (rectangular-histogram)
  ((bin-values
    :accessor contiguous-hist-bin-values
    :initarg :bin-values
    :initform nil
    :documentation "Nested arrays representing the bin values")))

;; I was thinking about making this a method on initialize-instance,
;; but it seems cleaner to keep this as a distinct function, since if
;; I wish to directly copy histogram contents the initialize-instance
;; method would have to handle these two cases separately, or I would
;; have to implement the copying in a strange way.
(defun make-contiguous-hist (dim-spec-plists &key empty-bin-value default-increment)
  "dim-spec-plist is a plist with the following
fields: :name :low :high :nbins, which specify the axis name,
bin-low-edge, bin-high-edge, and the number of bins on the axis
respectively.

Example usage: (make-contiguous-hist (list (list :name \"x\" :nbins
10 :low 50 :high 55))) would create a contiguous histogram with one
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
	 (size-list (mapcar #'first bin-specs))
	 (default-args (progn
			 (let ((result ()))
			   (when empty-bin-value
			     (push empty-bin-value result)
			     (push :empty-bin-value result))
			   (when default-increment
			     (push default-increment result)
			     (push :default-increment result))
			   result)))
	 (result (apply #'make-instance 'contiguous-histogram
			:ndims ndims
			:dim-names dim-names
			:bin-specs bin-specs
			default-args))
	 (bin-values (make-contiguous-hist-contents size-list
						    (hist-empty-bin-value result))))
    (setf (contiguous-hist-bin-values result) bin-values)
    result))

(defun make-chist (dim-spec-plists &key empty-bin-value default-increment)
  "Abbreviation for make-contiguous-hist"
  #k(make-contiguous-hist dim-spec-plists
                          &when-keys
                          empty-bin-value
                          default-increment))

(defmethod empty-copy ((h contiguous-histogram))
  (with-slots (empty-bin-value
               default-increment)
      h
    (make-chist (hist-dim-specs h)
                :empty-bin-value empty-bin-value
                :default-increment default-increment)))

(defmethod hist-total-integral ((hist contiguous-histogram))
  (let ((bin-values (hist-bin-values hist)))
    (sum (mapcar #'car bin-values))))

(defmethod hist-integrate ((histogram contiguous-histogram) &rest axes)
  (if axes
      (with-accessors ((ndims hist-ndims)
                       (dim-names hist-dim-names)
                       (empty-bin-value hist-empty-bin-value)
                       (default-increment hist-default-increment)
                       (bin-values contiguous-hist-bin-values)
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
                     (new-ndims (- ndims (length unique-sorted-indices)))
                     (new-dim-names (except-at dim-names unique-sorted-indices
                                               :uniquely-sorted t))
                     (new-bin-specs (except-at bin-specs unique-sorted-indices
                                               :uniquely-sorted t)))
                (make-instance 'contiguous-histogram
                               :ndims new-ndims
                               :dim-names new-dim-names
                               :empty-bin-value empty-bin-value
                               :default-increment default-increment
                               :bin-values (contiguous-hist-integrate-contents
                                            bin-values
                                            index-specs)
                               :bin-specs new-bin-specs)))))
      histogram))

;; note that hist-insert is a stateful function, since this is the
;; only efficient way to implement it
(defmethod hist-insert ((hist contiguous-histogram) datum &optional weight)
  (let ((data-list (mklist datum))
        (weight-factor (if weight
			   weight
			   (hist-default-increment hist))))
    (cond-setf (hist-point-ref hist data-list)
	       (+ (hist-point-ref hist data-list) weight-factor)
	       :place)
    hist))

(defmethod hist-index-ref ((hist contiguous-histogram) index-list)
  "Unchecked, assumes you know what the allowed index values are."
  (with-accessors ((bin-values contiguous-hist-bin-values))
      hist
    (apply #'tensor-ref bin-values index-list)))

(defmethod (setf hist-index-ref) (value (hist contiguous-histogram) index-list)
  "Unchecked, assumes you know what the allowed index values are."
  (with-accessors ((bin-values contiguous-hist-bin-values))
      hist
    (setf (apply #'tensor-ref bin-values index-list)
	  value)))

(defmethod hist-point-ref ((hist contiguous-histogram) data-list)
  "Checked access to the bin value via a point.  Returns nil if the
point is not inside the histogram domain."
  (with-accessors ((bin-specs rectangular-hist-bin-specs))
      hist
    (let ((bin-index (get-bin-index data-list bin-specs)))
      (when bin-index
	(hist-index-ref hist bin-index)))))

(defmethod (setf hist-point-ref) (value (hist contiguous-histogram) data-list)
  "Checked setf to the bin value via a point.  Does nothing & returns
nil if the point is not inside the histogram domain."
  (with-accessors ((bin-specs rectangular-hist-bin-specs)
		   (bin-values contiguous-hist-bin-values))
      hist
    (let ((bin-index (get-bin-index data-list bin-specs)))
      (when bin-index
	(setf (hist-index-ref hist bin-index)
	      value)))))

(defmethod hist-bin-values ((hist contiguous-histogram))
  (with-accessors ((bin-specs rectangular-hist-bin-specs)
		   (bin-values contiguous-hist-bin-values))
      hist
    (let* ((nbin-list (mapcar #'first bin-specs))
	   (bin-indices (apply #'cartesian-product
			       (mapcar (lambda (x) (range 0 (1- x))) nbin-list))))
      (mapcar (lambda (bin-index)
                (cons (hist-index-ref hist bin-index)
                      (get-bin-center bin-specs bin-index)))
	      bin-indices))))

;;; Generic Math functions:
;; Note that I am still not sure how to handle summing when the axis
;; limits and bin sizes are not the same.  It may be best to leave
;; this to the user and just provide naive arithmetic functions.
;;
;; In general you should be safe as long as your histograms have the
;; same axis limits & bin sizes.

(defun map-contiguous-hist (fn &rest histograms)
  "Defines the map operation over contiguous histograms."
  (let ((h1 (first histograms)))
    (with-accessors ((bin-specs rectangular-hist-bin-specs)
		     (ndims hist-ndims)
		     (dim-names hist-dim-names)
		     (empty-bin-value hist-empty-bin-value)
		     (default-increment hist-default-increment))
	h1
      (make-instance
       'contiguous-histogram
       :ndims ndims
       :dim-names dim-names
       :empty-bin-value empty-bin-value
       :default-increment default-increment
       :bin-specs bin-specs
       :bin-values
       (apply #'tensor-map
	      fn
	      (mapcar
	       #'contiguous-hist-bin-values
	       histograms))))))

(defmethod add ((h1 contiguous-histogram) (h2 contiguous-histogram))
  (map-contiguous-hist #'add h1 h2))

(defmethod sub ((h1 contiguous-histogram) (h2 contiguous-histogram))
  (map-contiguous-hist #'sub h1 h2))

(defmethod mult ((h1 contiguous-histogram) (h2 contiguous-histogram))
  (map-contiguous-hist #'mult h1 h2))

(defmethod div ((h1 contiguous-histogram) (h2 contiguous-histogram))
  (map-contiguous-hist #'div h1 h2))

(defmethod unary-div ((h1 contiguous-histogram))
  (map-contiguous-hist #'unary-div h1))

(defmethod protected-div ((h1 contiguous-histogram)
			  (h2 contiguous-histogram)
			  &key
			    (protected-value 0))
  (map-contiguous-hist
   (lambda (x y) (protected-div
                  x y
                  :protected-value
                  protected-value))
   h1 h2))

(defmethod protected-unary-div ((h contiguous-histogram)
				&key
				  (protected-value 0))
  (map-contiguous-hist
   (lambda (x) (protected-unary-div
                x
                :protected-value
                protected-value))
   h))

;;; Internal-usage functions

(defun contiguous-hist-integrate-contents (hist index-specs)
  "Assumes the index-specs are unique and sorted greatest to least."
  (reduce #'contiguous-hist-integrate-contents-worker index-specs
          :initial-value hist))

(defun contiguous-hist-integrate-contents-worker (hist index-spec)
  (if (zerop (if (listp index-spec)
		 (first index-spec)
		 index-spec))
      (reduce #'tensor-+ (if (listp index-spec)
			     (subseq hist (second index-spec) (1+ (third index-spec)))
			     hist))
      (map (type-of hist)
	   (lambda (h)
             (contiguous-hist-integrate-contents-worker
              h (if (listp index-spec)
                    (cons (1- (first index-spec))
                          (rest index-spec))
                    (1- index-spec))))
	   hist)))

(defun make-contiguous-hist-contents (size-list initial-value)
  (if size-list
      (let ((size (first size-list)))
	(if (single size-list)
	    (make-array (first size-list) :initial-element initial-value)
	    (let* ((result (make-array size)))
	      (loop for i from 0 below size
		 do (setf (elt result i)
			  (make-contiguous-hist-contents
			   (rest size-list) initial-value)))
	      result)))
      nil))

(defun point-in-bounds (hist point)
  (with-accessors ((bin-specs rectangular-hist-bin-specs))
      hist
    (let ((lower-bounds (mapcar #'second bin-specs))
	  (upper-bounds (mapcar #'third bin-specs)))
      (every (lambda (x y z) (and (<= x y)
                                  (< y z)))
	     lower-bounds point upper-bounds))))

(defmethod hist-reorder-dimensions ((histogram contiguous-histogram) dim-indices)
  (if (equal dim-indices (range 0 (length dim-indices)))
      histogram
      (let ((result
             (make-contiguous-hist (permute (hist-dim-specs histogram)
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

(defmethod type-constructor ((hist contiguous-histogram))
  #'make-contiguous-hist)
