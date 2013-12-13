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
			      selected-axis-indices)))
	(hist-integrate histogram integrate-axes)))))

(defgeneric hist-insert (histogram data-list &optional weight)
  (:documentation "Inserts a value specified by the list of data into
  the histogram; i.e. increments the bin value by weight (which
  defaults to 1 or whatever you set)."))

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

(defun get-dim-indices (dim-names axes)
  "Converts axes from a list of either index or name into a list of
indices by looking up the name when necessary."
  (mapcar
   (lambda (s)
     (if (stringp s)
         (position s dim-names :test #'equal)
         s))
   axes))
