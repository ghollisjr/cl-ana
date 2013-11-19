;;;; package.lisp

(defpackage :histogram
  (:use #:cl
	#:tensor
	#:list-utils
	#:macro-utils
	#:iter
	#:alexandria
	#:fitting)
  (:export :histogram
	   :contiguous-histogram
	   :sparse-histogram
	   :make-contiguous-hist
	   :make-sparse-hist
	   :contiguous-hist-map
	   :hist-ndims
	   :hist-dim-names
	   :hist-empty-bin-value
	   :hist-default-increment
	   :hist-integrate
	   :hist-project
	   :hist-insert
	   :hist-index-ref ; setf-able
	   :hist-point-ref ; setf-able
	   ;; :subhist ; subhist is just integration over a subset of the axis
	   :hist-bin-values))

(gmath:use-gmath :histogram)

;;; My idea for the structure is to have the histogram interface
;;; defined in this package.  Then I will specialize on it with the
;;; fixed-width-bin histogram.  On this I will build the contiguous
;;; and sparse histograms.  I'm not sure how I want to handle error
;;; bars.  Error bars seem to be value oriented and not specific to
;;; histograms or anything else really.  This makes me think I should
;;; define a numeric type which includes an error bar, and then I can
;;; use this as the data type for the histogram bin counts.  This
;;; error-value type can potentially be used with the fitting routines
;;; as well, as you could potentially define all the arithmetic
;;; operators so that they properly propogate errors as well as
;;; compute the result.  I know that if you do this for the bin count
;;; in a histogram with sqrt(n) as the error for bin value n, then
;;; each time you add a new member it is properly handled (yields
;;; sqrt(n+1) error).
