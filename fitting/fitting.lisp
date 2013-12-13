;;;; fitting.lisp

(in-package :fitting)

;;;; Provides function fit which is a high level interface to the GSLL
;;;; nonlinear-least-squares functionality.  Includes numeric
;;;; calculation of the jacobian instead of having to do tedious hand
;;;; calculations or write automatic/symbolic differentiation
;;;; routines.
;;;;
;;;; Also allows for weighted fitting if one uses err-num values as
;;;; the dependent variable values in the data to be fitted against
;;;; (uncertainty in the independent variable is not currently
;;;; supported, but theoretically this is outside the scope of the
;;;; technique itself.
;;;;
;;;; To be able to call the fit function for your data object, just
;;;; define a specialization on the generic function map->alist
;;;; which returns an alist mapping the independent values to the
;;;; dependent value (which must be either a regular common lisp
;;;; number or an err-num).

(defun alist-to-arrays (alist)
  (let ((xarray (map 'vector #'car alist))
	(yarray (map 'vector #'cdr alist)))
    (values xarray yarray)))

(defun list-to-grid (list)
  (let ((result (grid:make-foreign-array 'double-float :dimensions (length list))))
    (loop
       for l in list
       for i from 0
       do (setf (grid:aref result i) (float l 0d0)))
    result))

(defun grid-to-list (grid)
  (let ((dim (first (grid:dimensions grid))))
    (loop
       for i from 0 below dim
       collect (grid:aref grid i))))

(defun fit (data-source fn init-params &key
                                         (max-iterations 25)
                                         (prec 1.0d-6)
                                         (derivative-delta 1d-11))
  "Fits a function fn against data from data-source using the initial
parameters init-params.  Use err-num data type in the dependent
variable's value if you want to do a weighted least squares fit.

data-source: A generic object which has a map->alist function
defined for it.  This is the data which will be fitted against.

fn: A function which takes two arguments: 1. A fit parameter
list (must be a list), 2. The independent variable value which will
come from the data to be fitted against.

There is however one restriction when using err-num values as the
dependent variable value: You must either use err-num values for every
datum or none; I don't know of a good way to handle mixing err-num
values with non-err-num values which wouldn't be more cumbersome than
having the user decide.

init-params: a list of the initial parameter values.

The return values of fit are:

1. fn with the best-fit parameters applied,
2. The list of best-fit parameters,
3. The list of uncertainties in the best-fit parameters,
4. The value of chi^2/(degrees of freedom) for the fit,
5. The number of iterations it took to converge on the solution."
  (let* ((data (map->alist data-source))
	 (xlist (mapcar #'car data))
	 (ylist (mapcar #'cdr data))
	 (n-params (length init-params))
	 (n-data (length data))
	 (init-param-grid (list-to-grid init-params))
	 covariance
	 num-iterations)
    (let ((residual-fn
	   (if (subtypep (type-of (first ylist))
			 'err-num)
	       (lambda (param-list x y)
                 (/ (- (funcall fn param-list x)
                       (err-num-value y))
                    (err-num-error y)))
	       (lambda (param-list x y)
                 (- (funcall fn param-list x)
                    y)))))
      (labels ((residual (param-grid result-grid)
		 (let ((param-list (grid-to-list param-grid)))
                   (loop
                      for i from 0 below n-data
                      for x in xlist
                      for y in ylist
                      do (setf (grid:aref result-grid i)
                               (funcall residual-fn param-list x y)))))
	       (make-residual-jacobian (fn n-data n-params)
		 (lambda (param-grid jacobian)
                   (let ((param-list (grid-to-list param-grid)))
                     (loop
                        for i from 0 below n-data
                        for x in xlist
                        for y in ylist
                        do
                          (loop
                             for j from 0 below n-params
                             do
                               (let* ((changed-param-list
                                       (loop
                                          for k from 0
                                          for p in param-list
                                          collect (if (= k j)
                                                      (+ p derivative-delta)
                                                      p))))
                                 (setf (grid:aref jacobian i j)
                                       (/ (- (funcall fn changed-param-list x y)
                                             (funcall fn param-list x y))
                                          derivative-delta)))))))))
	(let* ((residual-jacobian
		(make-residual-jacobian residual-fn
					n-data
					n-params))
	       (fit
		(gsll:make-nonlinear-fdffit
		 gsll:+levenberg-marquardt+
		 (list n-data n-params)
		 (list #'residual
		       residual-jacobian
		       (lambda (x f jacobian)
                         (funcall #'residual x f)
                         (funcall residual-jacobian x jacobian)))
		 init-param-grid
		 nil)))
	  (macrolet ((fitx (i) `(grid:aref (gsll:solution fit) ,i))
		     (err (i) `(sqrt (grid:aref covariance ,i ,i))))
	    (loop
	       for iter from 0 below max-iterations
	       until
		 (and (plusp iter)
		      ;;(gsll:fit-test-delta fit prec prec))
		      (gsll:fit-test-delta fit 0d0 prec))
	       do
		 (gsll:iterate fit)
	       finally
		 (progn
		   (setf covariance (gsll:ls-covariance fit 0.0d0 covariance))
		   (setf num-iterations iter)))
	    (let* ((chi (gsll::norm-f fit))
		   (dof (- n-data n-params))
					;(c (max 1.0d0 (/ chi (sqrt dof))))) ;; not sure why
					;they made the cutoff
		   (c (/ chi (sqrt dof))))
              (let ((fit-params
                     (loop
                        for i from 0 below n-params
                        collect (fitx i)))
                    (fit-errors
                     (loop
                        for i from 0 below n-params
                        collect (* c (err i)))))
                (values (curry fn fit-params)
                        fit-params
                        fit-errors
                        (expt c 2)
                        num-iterations)))))))))
