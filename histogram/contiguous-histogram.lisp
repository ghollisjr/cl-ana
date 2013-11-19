;;;; contiguous-histogram.lisp

;;;; Implements histograms with binning which is both contiguous and
;;;; uniform.

(in-package :histogram)

(defclass contiguous-histogram (histogram)
  ((bin-values
    :accessor contiguous-hist-bin-values
    :initarg :bin-values
    :initform nil
    :documentation "Nested arrays representing the bin values")
   (bin-specs
    :accessor contiguous-hist-bin-specs
    :initarg :bin-specs
    :initform nil
    :documentation "List of number of bins, bin low edge, and bin high
    edge.  Should be computed from a bin specification plist and then
    stored in said order as a list (not a plist).")))

(defun make-contiguous-hist (dim-spec-plists &key empty-bin-value default-increment)
  "dim-spec-plist is a plist with the following
fields: :name :low :high :nbins, which specify the axis name,
bin-low-edge, bin-high-edge, and the number of bins on the axis
respectively.

Example usage: (make-contiguous-hist (list (list :name \"x\" :nbins
10 :low 50 :high 55))) would create a contiguous histogram with one
dimension named \"x\" with 10 bins, low bin edge 50 and high bin edge
55."
  (let* ((dim-names (mapcar #'(lambda (spec) (getf spec :name))
			    dim-spec-plists))
	 (ndims (length dim-names))
	 (bin-specs (mapcar #'(lambda (s)
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

(defmethod hist-integrate ((histogram contiguous-histogram) &rest axes)
  (with-accessors ((ndims hist-ndims)
		   (dim-names hist-dim-names)
		   (empty-bin-value hist-empty-bin-value)
		   (default-increment hist-default-increment)
		   (bin-values contiguous-hist-bin-values)
		   (bin-specs contiguous-hist-bin-specs))
      histogram
    (flet ((index-key (x)
	     (if (listp x)
		 (first x)
		 x)))
      (let* ((axis-name-or-indices (mapcar #'(lambda (x)
					       (if (listp x)
						   (first x)
						   x))
					   axes))
	     (dim-indices (get-dim-indices dim-names axis-name-or-indices))
	     (point-bounds
	      (mapcar #'(lambda (x)
			  (when (listp x)
			    (rest x)))
		      axes))
	     (index-bounds
	      (mapcar #'(lambda (point-bound bin-spec)
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
	     (index-specs (mapcar #'(lambda (x y)
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
	       #'(lambda (x y) (adjoin y x
				       :test #'equal
				       :key #'index-key))
	       sorted-index-specs
	       :initial-value ()))
	     (unique-sorted-indices
	      (mapcar #'(lambda (x)
			  (if (listp x)
			      (first x)
			      x))
		      unique-sorted-index-specs))
	     (new-ndims (- ndims (length unique-sorted-indices)))
	     (new-dim-names (except-at dim-names (reverse unique-sorted-indices)
				       :uniquely-sorted t))
	     (new-bin-specs (except-at bin-specs (reverse unique-sorted-indices)
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

;; note that hist-insert is a stateful function, since this is the
;; only efficient way to implement it
(defmethod hist-insert ((hist contiguous-histogram) data-list &optional weight)
  (let ((weight-factor (if weight
			   weight
			   (hist-default-increment hist))))
    (cond-setf (hist-point-ref hist data-list)
	       (+ (hist-point-ref hist data-list) weight-factor)
	       :place)))

(defmethod hist-index-ref ((hist contiguous-histogram) index-list)
  "Unchecked, assumes you know what the allowed index values are."
  (with-accessors ((bin-values contiguous-hist-bin-values))
      hist
    (reduce #'aref index-list :initial-value bin-values)))

(defmethod (setf hist-index-ref) (value (hist contiguous-histogram) index-list)
  "Unchecked, assumes you know what the allowed index values are."
  (with-accessors ((bin-values contiguous-hist-bin-values))
      hist
    (let* ((last-array
	    (reduce #'aref (butlast index-list) :initial-value bin-values))
	   (last-index (first (last index-list))))
      (setf (aref last-array last-index) value))))

(defmethod hist-point-ref ((hist contiguous-histogram) data-list)
  "Checked access to the bin value via a point.  Returns nil if the
point is not inside the histogram domain."
  (with-accessors ((bin-specs contiguous-hist-bin-specs)
		   (bin-values contiguous-hist-bin-values))
      hist
    (let ((bin-index (get-bin-index data-list bin-specs)))
      (when bin-index
	(reduce #'aref bin-index :initial-value bin-values)))))

(defmethod (setf hist-point-ref) (value (hist contiguous-histogram) data-list)
  "Checked setf to the bin value via a point.  Does nothing & returns
nil if the point is not inside the histogram domain."
  (with-accessors ((bin-specs contiguous-hist-bin-specs)
		   (bin-values contiguous-hist-bin-values))
      hist
    (let ((bin-index (get-bin-index data-list bin-specs)))
      (when bin-index
	(let ((last-array
	       (reduce #'aref (butlast bin-index) :initial-value bin-values))
	      (last-index (first (last bin-index))))
	  (setf (aref last-array last-index) value))))))

(defmethod hist-bin-values ((hist contiguous-histogram))
  (with-accessors ((bin-specs contiguous-hist-bin-specs)
		   (bin-values contiguous-hist-bin-values))
      hist
    (let* ((nbin-list (mapcar #'first bin-specs))
	   (bin-indices (apply #'cartesian-product
			       (mapcar #'(lambda (x) (range 0 (1- x))) nbin-list))))
      (mapcar #'(lambda (bin-index)
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
    (with-accessors ((bin-specs contiguous-hist-bin-specs)
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
   #'(lambda (x y) (protected-div
		    x y
		    :protected-value
		    protected-value))
   h1 h2))

(defmethod protected-unary-div ((h contiguous-histogram)
				&key
				  (protected-value 0))
  (map-contiguous-hist
   #'(lambda (x) (protected-unary-div
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
	   #'(lambda (h)
	       (contiguous-hist-integrate-contents-worker
		h (if (listp index-spec)
		      (cons (1- (first index-spec))
			    (rest index-spec))
		      (1- index-spec))))
	   hist)))

(defun make-contiguous-hist-contents (size-list initial-value)
  (if size-list
      (let ((size (first size-list)))
	(if (singletonp size-list)
	    (make-array (first size-list) :initial-element initial-value)
	    (let* ((result (make-array size)))
	      (loop for i from 0 below size
		 do (setf (elt result i)
			  (make-contiguous-hist-contents
			   (rest size-list) initial-value)))
	      result)))
      nil))

(defun point-in-bounds (hist point)
  (with-accessors ((bin-specs contiguous-hist-bin-specs))
      hist
    (let ((lower-bounds (mapcar #'second bin-specs))
	  (upper-bounds (mapcar #'third bin-specs)))
      (every #'(lambda (x y z) (and (<= x y)
				    (< y z)))
	     lower-bounds point upper-bounds))))
