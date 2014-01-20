;;;; histogram.lisp

;;;; Histogram interface.  Most of the interesting mathematical
;;;; features like summing, multiplying, scaling histograms are done
;;;; via the generic math (gmath) library.

(in-package :histogram)

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
  specified.  Axes can be specified using index or the dimension name
  (a string).

Also in order to support partial domain integration, each axis in the
axes list can also be a list of three values: the axis name or index,
the lower bound over which to integrate, and the upper bound."))

(defgeneric hist-project (histogram &rest axes)
  (:documentation "Projects the histogram onto specified axes by
  integrating over the other axes.  As with integrate, the axes can be
  specified using index or name.  Comes with a default implementation
  that should be good for most cases; specialize only if necessary.")
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

(defgeneric hist-insert (histogram datum &optional weight)
  (:documentation "Inserts a value specified by the datum (a list of
  values) into the histogram; i.e. increments the bin value by
  weight (which defaults to 1 or whatever you set)."))

(defgeneric hist-index-ref (histogram point)
  (:documentation "Like aref for an array, but for histograms using
  the index list."))

(defgeneric (setf hist-index-ref) (value histogram point)
  (:documentation "hist-index-ref is setf-able"))

(defgeneric hist-point-ref (histogram point)
  (:documentation "Like hist-index-ref but looks up the cell in which
  point would lie."))

(defgeneric (setf hist-point-ref) (value histogram point)
  (:documentation "hist-point-ref is setf-able"))

(defgeneric subhist (histogram &rest dim-subranges)
  (:documentation "Selects the histogram only along subranges
  specified via dim-subranges.  dim-subranges should be a list of axis
  index/name, lower bound, and upper bound in that order."))

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

;; Functional access to histograms:
(defun hist-map (fn hist)
  "hist-map maps the function fn over the histogram hist bin-by-bin.

fn should take as its first argument the bin value and the rest the
bin center values for each dimension of the histogram as keyword
parameters, and should return the new bin value for that bin; for a
bin to not be re-filled in the resulting histogram, return nil.

Note that a particularly useful strategy is to use &allow-other-keys
so that you do not have to worry about all the dimensions in the
histogram."
  (let* ((result
          (funcall (type-constructor hist)
                   (bin-spec-plists hist)
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

(defun hist-filter (fn hist)
  "Re-fills entries in the histogram only when fn returns non-nil.

fn should take as its first argument the bin count and the rest of the
arguments being the bin centers for each dimension of the histogram as
keyword arguments."
  (hist-map (lambda (count &rest xs)
              (when (apply fn count xs)
                count))
            hist))

(defun get-dim-indices (dim-names axes)
  "Converts axes from a list of either index or name into a list of
indices by looking up the name when necessary."
  (mapcar
   (lambda (s)
     (if (stringp s)
         (position s dim-names :test #'equal)
         s))
   axes))

;;;; Ease of use functions:

(defun hist-insert-list (histogram data-list)
  "Inserts each data list in data-lists into the histogram.  Accepts
data as either atom for 1-D or lists for any dimensionality."
  (loop
     for datum in data-list
     do (hist-insert histogram (mklist datum))))
