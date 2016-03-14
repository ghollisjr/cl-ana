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

(in-package :cl-ana.err-prop)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass err-num ()
    ((val
      :accessor err-num-value
      :initarg :value
      :initform 0)
     (err
      :accessor err-num-error
      :initarg :error
      :initform 0)))

  ;; Old method:
  ;; (defmethod print-object ((e err-num) stream)
  ;;   (labels ((rec (e result)
  ;;              (if (subtypep (type-of e)
  ;;                            'err-num)
  ;;                  (with-slots (val err)
  ;;                      e
  ;;                    (rec err (cons val result)))
  ;;                  (nreverse (cons e result)))))
  ;;     (format stream "~a" (cons "+-" (rec e nil)))))

  ;; New method:

  (defun err-num-list (e)
    "Returns list of values and errors for an err-num with nested
errors."
    (labels ((rec (e result)
               (if (subtypep (type-of e)
                             'err-num)
                   (with-slots (val err)
                       e
                     (rec err (cons val result)))
                   (nreverse (cons e result)))))
      (rec e nil)))
  
  (defmethod print-object ((e err-num) stream)
    (format stream "#~~~a" (err-num-list e)))

  (defun err-num-transformer-reader-macro (stream subchar arg)
    (let* ((expr (read stream t)))
      (apply #'+- expr)))

  (set-dispatch-macro-character
   #\# #\~ #'err-num-transformer-reader-macro)

  (defmethod make-load-form ((self err-num) &optional environment)
    `(+- ,@(err-num-list self)))
  
  ;; The constructor for err-nums:
  (defun +- (&rest val-errs)
    "Constructs a numerical value along with errors.  Note that the
structure allows for errors in errors to arbitrary depth."
    (when val-errs
      (let ((val (first val-errs))
            (errs (rest val-errs)))
        (if errs
            (make-instance 'err-num
                           :value val
                           :error (apply #'+- errs))
            val))))

  (defun err-num-+ (&rest err-nums)
    (+- (reduce #'+ (mapcar #'err-num-value err-nums))
        (sum-in-quadrature
         (mapcar #'err-num-error err-nums))))

  (defmethod add ((x err-num) (y err-num))
    (+- (apply #'add (mapcar #'err-num-value (list x y)))
        (sum-in-quadrature
         (mapcar #'err-num-error (list x y)))))

  (defmethod-commutative add ((x err-num) (y number))
    (add x (+- y 0)))

  (defun err-num-- (&rest err-nums)
    (+- (apply #'- (mapcar #'err-num-value err-nums))
        (sum-in-quadrature
         (mapcar #'err-num-error err-nums))))

  (defmethod sub ((x err-num) (y err-num))
    (+- (apply #'sub
               (mapcar #'err-num-value (list x y)))
        (sum-in-quadrature
         (mapcar #'err-num-error (list x y)))))

  (defmethod sub ((x err-num) (y number))
    (sub x (+- y 0)))

  (defmethod sub ((x number) (y err-num))
    (sub (+- x 0) y))

  (defmethod unary-sub ((x err-num))
    (with-accessors ((val err-num-value)
                     (err err-num-error))
        x
      (+- (unary-sub val) err)))

  (defun err-num-* (&rest err-nums)
    (let* ((values (mapcar #'err-num-value err-nums))
           (errors (mapcar #'err-num-error err-nums))
           (relative-errors (mapcar #'div errors values))
           (result-value (apply #'mult values)))
      (+- result-value
          (* result-value
             (sum-in-quadrature
              relative-errors)))))

  (defmethod mult ((x err-num) (y err-num))
    (let* ((values (mapcar #'err-num-value (list x y)))
           (errors (mapcar #'err-num-error (list x y)))
           (relative-errors (mapcar #'div errors values))
           (result-value (apply #'mult values)))
      (+- result-value
          (mult result-value
                (sum-in-quadrature
                 relative-errors)))))

  (defmethod-commutative mult ((x err-num) (y number))
    (mult x (+- y 0)))

  (defun err-num-/ (&rest err-nums)
    (let* ((values (mapcar #'err-num-value err-nums))
           (errors (mapcar #'err-num-error err-nums))
           (relative-errors (mapcar #'/ errors values))
           (result-value (reduce #'/ values)))
      (+- result-value
          (* result-value
             (sum-in-quadrature
              relative-errors)))))

  (defmethod div ((x err-num) (y err-num))
    (let* ((arglist (list x y))
           (values (mapcar #'err-num-value arglist))
           (errors (mapcar #'err-num-error arglist))
           (relative-errors (mapcar #'div errors values))
           (result-value (apply #'div values)))
      (+- result-value
          (* result-value
             (sum-in-quadrature
              relative-errors)))))

  (defmethod div ((x err-num) (y number))
    (div x (+- y 0)))

  (defmethod div ((x number) (y err-num))
    (div (+- x 0) y))

  (defmethod unary-div ((x err-num))
    (let ((result-value (unary-div (err-num-value x))))
      (+- result-value
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
      (+- result-value
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
      (+- result-value
          (sum-in-quadrature
           (list result-error1 result-error2)))))

  (defmethod expt ((x number) (y err-num))
    (expt (+- x 0) y))

  (defmethod expt ((x err-num) (y number))
    (expt x (+- y 0)))

  (defmethod exp ((err-num err-num))
    (let ((result-value
           (exp (err-num-value err-num))))
      (+- result-value
          (* result-value
             (err-num-error err-num)))))

  (defmethod log ((err-num err-num))
    (with-accessors ((val err-num-value)
                     (err err-num-error))
        err-num
      (+- (log val)
          (/ err val))))

  (defmethod sin ((err-num err-num))
    (with-accessors ((val err-num-value)
                     (err err-num-error))
        err-num
      (+- (sin val)
          (* err
             (abs (cos val))))))

  (defmethod cos ((err-num err-num))
    (with-accessors ((val err-num-value)
                     (err err-num-error))
        err-num
      (+- (cos val)
          (* err
             (abs (sin val))))))

  (defmethod tan ((err-num err-num))
    (with-accessors ((val err-num-value)
                     (err err-num-error))
        err-num
      (+- (tan val)
          (/ err
             (expt (cos val) 2)))))

  (defmethod sinh ((err-num err-num))
    (with-accessors ((val err-num-value)
                     (err err-num-error))
        err-num
      (+- (sinh val)
          (* err
             (cosh val)))))

  (defmethod cosh ((err-num err-num))
    (with-accessors ((val err-num-value)
                     (err err-num-error))
        err-num
      (+- (cosh val)
          (* err
             (abs (sinh val))))))

  (defmethod tanh ((err-num err-num))
    (with-accessors ((val err-num-value)
                     (err err-num-error))
        err-num
      (+- (tanh val)
          (/ err
             (expt (cosh val) 2)))))

  (defun sum-in-quadrature (xs)
    (sqrt
     (apply #'+
            (mapcar (lambda (x) (expt x 2))
                    xs)))))
