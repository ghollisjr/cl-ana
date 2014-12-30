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

;;;; Histogram interface.  Most of the interesting mathematical
;;;; features like summing, multiplying, scaling histograms are done
;;;; via the generic math (gmath) library.

(in-package :cl-ana.histogram)

(defclass histogram ()
  ((ndims
    :accessor hist-ndims
    :initarg :ndims
    :initform 0
    :documentation "Number of dimensions of the histogram")
   (dim-names
    :accessor hist-dim-names
    :initarg :dim-names
    :initform ()
    :documentation "Names of the dimension axes")
   (empty-bin-value
    :accessor hist-empty-bin-value
    :initarg :empty-bin-value
    :initform 0
    :documentation "Default value for a bin which has had no events
    added")
   (default-increment
       :accessor hist-default-increment
     :initarg :default-increment
     :initform 1
     :documentation "Default amount by which bins are incremented;
    i.e. unless overridden by a weight factor.")))

(defgeneric hist-total-integral (histogram)
  (:documentation "Returns the number of entries in the histogram,
  i.e. the full integral of the histogram"))

(defgeneric hist-integrate (histogram &rest axes)
  (:documentation "Integrates the histogram along the dimensions/axes
  specified.  Axes are specified using the lispified dimension name
  (a string); specifying by index is for internal use but can be done.

Also in order to support partial domain integration, each axis in the
axes list can also be a list of three values: the axis name, the lower
bound over which to integrate, and the upper bound."))

(defgeneric hist-project (histogram &rest axes)
  (:documentation "Projects the histogram onto specified axes by
  integrating over the other axes.  As with integrate, the axes are
  specified using name.  Comes with a default implementation that
  should be good for most cases; specialize only if necessary.")
  (:method (histogram &rest axes)
    (with-accessors ((dim-names hist-dim-names)
		     (ndims hist-ndims))
	histogram
      (let* ((selected-axis-indices
	      (get-dim-indices dim-names axes))
	     (integrate-axes
	      (set-difference (range 0 (1- ndims))
			      selected-axis-indices))
             (disordered-hist
              (apply #'hist-integrate histogram integrate-axes))
             (new-selected-axis-indices
              (condense-indices selected-axis-indices)))
        (hist-reorder-dimensions
         disordered-hist
         new-selected-axis-indices)))))

(defgeneric hist-reorder-dimensions (histogram dim-indices)
  (:documentation "Re-arranges the data in the histogram so that the
  dimensions are permuted according to dim-indices."))

(defgeneric hist-insert (histogram datum &optional weight)
  (:documentation "Inserts a value specified by the datum (An atom for
  1-D or a list of values for more dimensions) into the histogram;
  i.e. increments the bin value by weight (which defaults to 1 or
  whatever you set).  Returns histogram (though it does change the
  histogram state)."))

;;;; Not sure if index-ref functions are appropriate for all types of
;;;; histograms
(defgeneric hist-index-ref (histogram indices)
  (:documentation "Like aref for an array, but for histograms using
  the index list."))

(defgeneric (setf hist-index-ref) (value histogram indices)
  (:documentation "hist-index-ref is setf-able"))

(defgeneric hist-point-ref (histogram point)
  (:documentation "Like hist-index-ref but looks up the cell in which
  point would lie.")
  ;; default method for non-histograms:
  (:method (x point)
    x))

(defgeneric (setf hist-point-ref) (value histogram point)
  (:documentation "hist-point-ref is setf-able"))

(defgeneric hist-bin-values (histogram)
  (:documentation "Returns a list of bin values consed to the bin
  center of the bin"))

(defmethod map->alist ((hist histogram))
  "Since fitting needs 1-D histograms to give not a list with the cdr
being a singleton list but a cons with the cdr being the actual bin
center, we have to do some footwork here."
  (let* ((hist-bin-values (hist-bin-values hist))
	 (alist-maker
	  (if (single (cdr (first hist-bin-values)))
	      (lambda (x)
                (let ((car (car x))
                      (cdr (first (cdr x))))
                  (cons cdr car)))
	      (lambda (x)
                (let ((car (car x))
                      (cdr (cdr x)))
                  (cons cdr car))))))
    (mapcar alist-maker
	    (hist-bin-values hist))))

(defgeneric hist-slice (hist &rest dims)
  (:documentation "Slices up the histogram along each dimension in
  dims.  Returns a hash table mapping the bin center list for each
  dimension in dims to a histogram of the same kind as hist which has
  only the leftover dimensions."))

(defun getslice (slices bin)
  "Function for finding a slice from (hist-slice hist) via minimizing
the sum of squares of the vector difference between bin and each slice
center."
  (ensure-lists (bin)
    (gethash (minimum (hash-keys slices)
                      :key (lambda (s)
                             (sum (expt (- s bin) 2))))
             slices)))

;; Functional access to histograms:
(defun hist-map (fn hist)
  "hist-map maps the function fn over the histogram hist bin-by-bin.

fn should take as its first argument the bin value and the rest the
bin center values for each dimension of the histogram as keyword
parameters.  The keyword arguments should be the lispified dimension
names of the histogram.  fn should return the new bin value for that
bin; for a bin to not be re-filled in the resulting histogram, return
nil.

Note that a particularly useful strategy is to use &allow-other-keys
so that you do not have to worry about all the dimensions in the
histogram."
  (let* ((result
          (funcall (type-constructor hist)
                   (hist-dim-specs hist)
                   :empty-bin-value
                   (hist-empty-bin-value hist)
                   :default-increment
                   (hist-default-increment hist)))
         (dim-names (hist-dim-names hist))
         (dim-keywords
          (mapcar (compose #'keywordify #'lispify)
                  dim-names)))
    (reduce (lambda (h x)
              (when x
                (hist-insert h
                             (car x)
                             (cdr x)))
              h)
            (mapcar
             (lambda (x)
               (let* ((key-args (mapcan #'list dim-keywords (rest x)))
                      (count (first x))
                      (result
                       (apply fn count key-args)))
                 (when result
                   (cons (rest x)
                         result))))
             (hist-bin-values hist))
            :initial-value result)))

;; I'm keeping the first argument as the bin count since I don't want
;; to clobber any dimensions of the histogram which might wish to be
;; called count or anything else I would happen to choose.
(defun hist-filter (fn hist)
  "Re-fills entries in the histogram only when fn returns non-nil.

fn should take as its first argument the bin count and the rest of the
arguments being the bin centers for each dimension of the histogram as
keyword arguments.  The keyword arguments should be the lispified
dimension names of the histogram."
  (hist-map (lambda (count &rest xs)
              (when (apply fn count xs)
                count))
            hist))

;;;; Ease of use functions:

(defun hist-insert-list (histogram data-list)
  "Inserts each data list in data-lists into the histogram.  Accepts
data as either atom for 1-D or lists for any dimensionality."
  (loop
     for datum in data-list
     do (hist-insert histogram (mklist datum))))

;;;; Abbreviations:

(defun hins (histogram datum &optional weight)
  "Abbreviation for hist-insert"
  (apply #'hist-insert
         histogram
         datum
         (when weight (list weight))))

(defun htint (histogram)
  "Abbreviation for hist-total-integral"
  (hist-total-integral histogram))

(defun hint (histogram &rest axes)
  "Abbreviation for hist-integrate"
  (apply #'hist-integrate histogram axes))

(defun hproj (histogram &rest axes)
  "Abbreviation for hist-project"
  (apply #'hist-project histogram axes))

(defun hslice (histogram &rest axes)
  "Abbreviation for hist-slice"
  (apply #'hist-slice histogram axes))

(defun hiref (histogram indices)
  "Abbreviation for hist-index-ref"
  (hist-index-ref histogram indices))

(defun (setf hiref) (value histogram indices)
  (setf (hist-index-ref histogram indices)
        value))

(defun hpref (histogram point)
  "Abbreviation for hist-point-ref"
  (hist-point-ref histogram point))

(defun (setf hpref) (value histogram point)
  (setf (hist-point-ref histogram point)
        value))

(defun hbv (histogram)
  "Abbreviation for hist-bin-values"
  (hist-bin-values histogram))

(defun hdn (histogram)
  "Abbreviation for hist-dim-names"
  (hist-dim-names histogram))

;;;; Generic math: (still need to handle functions with keyword
;;;; arguments)

(defmacro defhistmath-unary (fname)
  (with-gensyms (a count centers a-bin-values result)
    `(defmethod ,fname ((,a histogram))
       (let ((,a-bin-values
              (hist-bin-values ,a))
             (,result (copy-hist ,a t)))
         (loop
            for (,count . ,centers) in ,a-bin-values
            do (hist-insert ,result
                            ,centers
                            (,fname ,count)))
         ,result))))

(defun empty-set (&optional (test 'equal))
  (make-hash-table :test test))

(defun set-insert (set object)
  (multiple-value-bind (val p)
      (gethash object set)
    (when (not p)
      (setf (gethash object set)
            t))
    set))

(defun set->list (set)
  (loop for x being the hash-keys in set
     collecting x))

(defmacro defhistmath-binary (fname)
  (with-gensyms (a
                 b
                 res
                 count
                 all-centers
                 centers
                 a-bin-values
                 b-bin-values
                 result)
    (let ((lrbody
           `(let ((,all-centers
                   (cl-ana.histogram::set->list
                    (let ((,res (cl-ana.histogram::empty-set)))
                      (reduce #'cl-ana.histogram::set-insert
                              (mapcar #'cdr (hbv ,a))
                              :initial-value ,res)
                      (reduce #'cl-ana.histogram::set-insert
                              (mapcar #'cdr (hbv ,b))
                              :initial-value ,res))))
                  (,result (copy-hist ,a t)))
              (loop
                 for ,centers in ,all-centers
                 do (hist-insert ,result
                                 ,centers
                                 (,fname (hist-point-ref ,a ,centers)
                                         (hist-point-ref ,b ,centers))))
              ,result))
          (lbody
           `(let ((,a-bin-values
                   (hist-bin-values ,a))
                  (,result (copy-hist ,a t)))
              (loop
                 for (,count . ,centers) in ,a-bin-values
                 do (hist-insert ,result
                                 ,centers
                                 (,fname ,count (hist-point-ref ,b ,centers))))
              ,result))
          (rbody
           `(let ((,b-bin-values
                   (hist-bin-values ,b))
                  (,result (copy-hist ,b t)))
              (loop
                 for (,count . ,centers) in ,b-bin-values
                 do (hist-insert ,result
                                 ,centers
                                 (,fname (hist-point-ref ,a ,centers) ,count)))
              ,result)))
      `(progn
         (defmethod ,fname ((,a histogram) (,b histogram))
           ,lrbody)
         (defmethod ,fname ((,a histogram) ,b)
           ,lbody)
         (defmethod ,fname (,a (,b histogram))
           ,rbody)))))

(defun defhistmaths ()
  (loop
     for fname being the hash-keys in *gmath-generic-map*
     for arglist being the hash-values in *gmath-generic-map*
     do
       (case (length arglist)
         (1
          (eval `(defhistmath-unary ,fname)))
         (2
          (eval `(defhistmath-binary ,fname))))))

(defhistmaths)

(defmethod protected-div ((a histogram) (b histogram)
                          &key (protected-value 0))
  (let ((all-centers
         (cl-ana.histogram::set->list
          (let ((res (cl-ana.histogram::empty-set)))
            (reduce #'cl-ana.histogram::set-insert
                    (mapcar #'cdr (hbv a))
                    :initial-value res)
            (reduce #'cl-ana.histogram::set-insert
                    (mapcar #'cdr (hbv b))
                    :initial-value res))))
        (result (copy-hist a t)))
    (loop
       for centers in all-centers
       do (hist-insert result
                       centers
                       (protected-div (hist-point-ref a centers)
                                      (hist-point-ref b centers)
                                      :protected-value
                                      protected-value)))
    result))

(defgeneric copy-hist (hist &optional empty-p)
  (:documentation "Returns a filled copy of hist for null empty-p,
  otherwise returns empty copy")
  (:method (hist &optional empty-p)
    (hist-map (if empty-p
                  (constantly nil)
                  (lambda (count &key &allow-other-keys)
                    count))
              hist)))

;; ease of use function:
(defun bin (data
            &key
              dim-names
              mins
              maxs
              nbins
              empty-bin-value
              default-increment)
  "Creates a sparse histogram determining min, max, and number of bins
from the data.

data can be a list of atoms for 1-D data or a list of lists for any
dimensionality.

dim-names is an optional list of dimension names

mins, maxs, and nbins can be alists mapping from dimension name/index
to a value, or if all dimensions have a value specified, just a list
of values taken to be given in order of dimension.

empty-bin-value and default-increment will be supplied to the
histogram if present."
  (when data
    (let* ((dimensionality
            (let ((d (first data)))
              (if (atom d)
                  1
                  (length d))))
           (dim-names
            (if dim-names
                dim-names
                (loop for i below dimensionality
                   collect nil))))
      (flet ((mkspecs (specs)
               (if specs
                   (if (consp (first specs))
                       ;; alists
                       (labels ((rec (spcs names result)
                                  ;; spcs should be a hash-table mapping
                                  ;; from whatever is in names to a value
                                  (if names
                                      (let ((spec (gethash (first names)
                                                           spcs)))
                                        (rec spcs (rest names) (cons spec result)))
                                      (nreverse result))))
                         (rec (map->hash-table specs 'equal)
                              (if dim-names
                                  dim-names
                                  (range 0 (1- dimensionality)))
                              nil))
                       ;; full spec list
                       specs)
                   (loop for i below dimensionality
                      collect nil))))
        ;; each spec has either an explicit value or nil for each
        ;; dimension
        (let ((min-specs (mkspecs mins))
              (max-specs (mkspecs maxs))
              (nbin-specs (mkspecs nbins))
              (data-per-dim
               (transpose (mapcar #'mklist data))))
          (when (or (not (length-equal min-specs dimensionality))
                    (not (length-equal max-specs dimensionality))
                    (not (length-equal nbin-specs dimensionality)))
            (error "Inconsistent histogram dimensionality"))
          ;; final version of mins, maxs and nbins
          (let* ((hist-specs
                  (loop
                     for min in min-specs
                     for max in max-specs
                     for n in nbin-specs
                     for name in dim-names
                     for d in data-per-dim
                     collect
                       (let ((mi
                              (if min
                                  min
                                  (minimum d)))
                             (ma
                              (if max
                                  max
                                  (maximum d)))
                             (n
                              (if n n (ceiling (sqrt (length d))))))
                         (let (real-min real-max)
                           (if (and min
                                    max)
                               ;; min & max both explicit
                               (progn
                                 (setf real-min min)
                                 (setf real-max max))
                               ;; need delta
                               (let ((delta (/ (- ma mi) n)))
                                 (setf real-min
                                       (if min
                                           min
                                           (- mi (* 0.5 delta))))
                                 (setf real-max
                                       (if max
                                           max
                                           (+ ma (* 0.5 delta))))))
                           (list :low real-min
                                 :high real-max
                                 :nbins n
                                 :name name)))))
                 (result
                  (apply #'make-sparse-hist
                         hist-specs
                         (when-keywords
                           empty-bin-value
                           default-increment))))
            (loop
               for d in data
               do (hins result (mklist d)))
            result))))))

;;;; Internal use:

(defun condense-indices (indices)
  (let* ((sorted (sort (copy-list indices) #'<))
         (map
          (let ((result (make-hash-table :test 'equal)))
            (loop
               for i from 0
               for s in sorted
               do (setf (gethash s result)
                        i))
            result)))
    (mapcar (lambda (x)
              (gethash x map))
            indices)))

(defun get-dim-indices (dim-names axes)
  "Converts axes from a list of either index or name into a list of
indices by looking up the name when necessary."
  (mapcar
   (lambda (s)
     (if (stringp s)
         (position s dim-names :test #'equal)
         s))
   axes))
