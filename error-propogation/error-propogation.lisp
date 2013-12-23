;;;; error-propogation.lisp

;;; Until I have an object-oriented math library written which
;;; replaces the unfortunate common lisp non-generic number system &
;;; functions, I won't be able to do interesting things like have
;;; err-nums as values inside of err-nums (only really makes sense
;;; when the error is an err-num, i.e. uncertainty of uncertainty).

(in-package :err-prop)

;;; Reader macro: #e(value error) yields an err-num with value and error
(defun err-num-transformer-reader-macro (stream subchar arg)
  (let* ((expr (read stream t)))
    `(make-err-num ,@expr)))

(set-dispatch-macro-character
 #\# #\e #'err-num-transformer-reader-macro)

(defclass err-num ()
  ((val
    :accessor err-num-value
    :initarg :value
    :initform 0)
   (err
    :accessor err-num-error
    :initarg :error
    :initform 0)))

(defvar *err-num-pretty-print* t
  "Tells print-object whether to use the +- version of printing or the
  direct reader macro version #e(...)")

(defmethod print-object ((e err-num) stream)
  (labels ((rec (e result)
	     (if (subtypep (type-of e)
			   'err-num)
		 (with-slots (val err)
		     e
		   (rec err (cons val result)))
		 (nreverse (cons e result)))))
    (if *err-num-pretty-print*
	(with-accessors ((val err-num-value)
			 (err err-num-error))
	    e
	  (format stream "~a +- ~a" val err))
	(format stream "#e~a" (rec e nil)))))

(defun make-err-num (&rest val-errs)
  "Constructs a numerical value along with errors.  Note that the
structure allows for errors in errors to arbitrary depth."
  (when val-errs
    (let ((val (first val-errs))
	  (errs (rest val-errs)))
      (if errs
	  (make-instance 'err-num
			 :value val
			 :error (apply #'make-err-num errs))
	  val))))

(defun err-num-+ (&rest err-nums)
  (make-err-num (reduce #'+ (mapcar #'err-num-value err-nums))
		(sum-in-quadrature
                 (mapcar #'err-num-error err-nums))))

(defmethod add ((x err-num) (y err-num))
  (make-err-num (apply #'add (mapcar #'err-num-value (list x y)))
		(sum-in-quadrature
		 (mapcar #'err-num-error (list x y)))))

(defmethod-commutative add ((x err-num) (y number))
  (add x (make-err-num y 0)))

(defun err-num-- (&rest err-nums)
  (make-err-num (apply #'- (mapcar #'err-num-value err-nums))
		(sum-in-quadrature
		 (mapcar #'err-num-error err-nums))))

(defmethod sub ((x err-num) (y err-num))
  (make-err-num (apply #'sub
		       (mapcar #'err-num-value (list x y)))
		(sum-in-quadrature
		 (mapcar #'err-num-error (list x y)))))

(defmethod sub ((x err-num) (y number))
  (sub x (make-err-num y 0)))

(defmethod sub ((x number) (y err-num))
  (sub (make-err-num x 0) y))

(defmethod unary-sub ((x err-num))
  (with-accessors ((val err-num-value)
		   (err err-num-error))
      x
    (make-err-num (unary-sub val) err)))

(defun err-num-* (&rest err-nums)
  (let* ((values (mapcar #'err-num-value err-nums))
	 (errors (mapcar #'err-num-error err-nums))
	 (relative-errors (mapcar #'div errors values))
	 (result-value (apply #'mult values)))
    (make-err-num result-value
		  (* result-value
		     (sum-in-quadrature
		      relative-errors)))))

(defmethod mult ((x err-num) (y err-num))
  (let* ((values (mapcar #'err-num-value (list x y)))
	 (errors (mapcar #'err-num-error (list x y)))
	 (relative-errors (mapcar #'div errors values))
	 (result-value (apply #'mult values)))
    (make-err-num result-value
		  (mult result-value
			(sum-in-quadrature
			 relative-errors)))))

(defmethod-commutative mult ((x err-num) (y number))
  (mult x (make-err-num y 0)))

(defun err-num-/ (&rest err-nums)
  (let* ((values (mapcar #'err-num-value err-nums))
	 (errors (mapcar #'err-num-error err-nums))
	 (relative-errors (mapcar #'/ errors values))
	 (result-value (reduce #'/ values)))
    (make-err-num result-value
		  (* result-value
		     (sum-in-quadrature
		      relative-errors)))))

(defmethod div ((x err-num) (y err-num))
  (let* ((arglist (list x y))
	 (values (mapcar #'err-num-value arglist))
	 (errors (mapcar #'err-num-error arglist))
	 (relative-errors (mapcar #'div errors values))
	 (result-value (apply #'div values)))
    (make-err-num result-value
		  (* result-value
		     (sum-in-quadrature
		      relative-errors)))))

(defmethod div ((x err-num) (y number))
  (div x (make-err-num y 0)))

(defmethod div ((x number) (y err-num))
  (div (make-err-num x 0) y))

(defmethod unary-div ((x err-num))
  (let ((result-value (unary-div (err-num-value x))))
    (make-err-num result-value
		  (mult result-value (err-num-error x)))))

(defmethod protected-div ((x number) (y err-num)
			  &key
			    (protected-value 0))
  (if (zerop (err-num-value y))
      protected-value
      (div x y)))

(defmethod protected-unary-div ((x err-num)
				&key
				  (protected-value 0))
  (if (zerop (err-num-value x))
      protected-value
      (unary-div x)))

(defmethod sqrt ((err-num err-num))
  (let ((result-value (sqrt (err-num-value err-num))))
    (make-err-num result-value
                  (/ (err-num-error err-num)
                     2
                     result-value))))

(defmethod expt ((x err-num)
		 (y err-num))
  (let* ((value1 (err-num-value x))
	 (value2 (err-num-value y))
	 (error1 (err-num-error x))
	 (error2 (err-num-error y))
	 (result-value (expt value1 value2))
	 (result-error1
	  (* error1
	     value2
	     (expt value1 (- value2 1))))
	 (result-error2
	  ;; had to handle limits more carefully with this
	  (if (zerop value1)
	      0
	      (* error2
		 result-value
		 (log value1)))))
    (make-err-num result-value
		  (sum-in-quadrature
		   (list result-error1 result-error2)))))

(defmethod expt ((x number) (y err-num))
  (expt (make-err-num x 0) y))

(defmethod expt ((x err-num) (y number))
  (expt x (make-err-num y 0)))

(defmethod exp ((err-num err-num))
  (let ((result-value
	 (exp (err-num-value err-num))))
    (make-err-num result-value
		  (* result-value
		     (err-num-error err-num)))))

(defmethod log ((err-num err-num))
  (with-accessors ((val err-num-value)
		   (err err-num-error))
      err-num
    (make-err-num (log val)
		  (/ err val))))

(defmethod sin ((err-num err-num))
  (with-accessors ((val err-num-value)
		   (err err-num-error))
      err-num
    (make-err-num (sin val)
		  (* err
		     (abs (cos val))))))

(defmethod cos ((err-num err-num))
  (with-accessors ((val err-num-value)
		   (err err-num-error))
      err-num
    (make-err-num (cos val)
		  (* err
		     (abs (sin val))))))

(defmethod tan ((err-num err-num))
  (with-accessors ((val err-num-value)
		   (err err-num-error))
      err-num
    (make-err-num (tan val)
		  (/ err
		     (expt (cos val) 2)))))

(defmethod sinh ((err-num err-num))
  (with-accessors ((val err-num-value)
		   (err err-num-error))
      err-num
    (make-err-num (sinh val)
		  (* err
		     (cosh val)))))

(defmethod cosh ((err-num err-num))
  (with-accessors ((val err-num-value)
		   (err err-num-error))
      err-num
    (make-err-num (cosh val)
		  (* err
		     (abs (sinh val))))))

(defmethod tanh ((err-num err-num))
  (with-accessors ((val err-num-value)
		   (err err-num-error))
      err-num
    (make-err-num (tanh val)
		  (/ err
		     (expt (cosh val) 2)))))

(defun sum-in-quadrature (xs)
  (sqrt
   (apply #'+
	  (mapcar (lambda (x) (expt x 2))
		  xs))))
