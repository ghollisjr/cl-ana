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

(in-package :cl-ana.quantity)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass quantity ()
    ((scale
      :initform 0
      :initarg :scale
      :accessor quantity-scale
      :documentation "The numerical coefficient expressing the number of
    units the quantity represents.")
     (unit
      :initform 1
      :initarg :unit
      :accessor quantity-unit
      :documentation "The unit the quantity is measured via.")))

  (defmethod initialize-instance :after
    ((q quantity) &key)
    (with-accessors ((unit quantity-unit))
        q
      unit
      (setf unit (unit-standard-order unit))))

  ;; make-load-form was causing problems:
  (defmethod make-load-form ((self quantity) &optional environment)
    `(make-instance 'quantity
                    :scale ',(quantity-scale self)
                    :unit ',(quantity-unit self)))

  ;; Defunct method which didn't use the reader-macro.
  ;;
  ;; (defmethod print-object ((q quantity) stream)
  ;;   (format stream "(* ~{~S~^ ~})" ; ~S tries to print READable formatting.
  ;;           (cons (quantity-scale q)
  ;;                 (loop
  ;;                    for u in (mklist (quantity-unit q))
  ;;                    collecting
  ;;                      (if (listp u)
  ;;                          `(expt ,@u)
  ;;                          u)))))

  (defun reader-macro-units->quantity (unit-list)
    (apply #'*
           (mapcar (lambda (u)
                     (if (listp u)
                         (apply #'expt u)
                         u))
                   unit-list)))

  ;; Experimental version
  ;;
  ;; (defun quantity-transformer-reader-macro (stream subchar arg)
  ;;   (let* ((expr (read stream t)))
  ;;     `(* ',(first expr)
  ;;         (reader-macro-units->quantity
  ;;          ,(cons 'list
  ;;                 (loop
  ;;                    for e in (rest expr)
  ;;                    collecting
  ;;                      (if (listp e)
  ;;                          (cons 'list
  ;;                                e)
  ;;                          e)))))))

  ;; Read-time version:
  (defmethod print-object ((q quantity) stream)
    (format stream "#q(~{~S~^ ~})" ; ~S tries to print READable formatting.
            (cons (quantity-scale q)
                  (mklist (quantity-unit q)))))

  ;; Function for distrubuting units across a sequence
  (defun distribute-units (q)
    (let* ((scale (quantity-scale q))
           (unit (quantity-unit q)))
      (if (typep scale 'sequence)
          `',(apply #'* scale (mklist unit))
          q)))

  (defun quantity-transformer-reader-macro (stream subchar arg)
    (let* ((expr (read stream t))
           (scale (first expr))
           (unit (if (single (rest expr))
                     (first (rest expr))
                     (rest expr))))
      ;; Handle sequences
      (distribute-units
       (make-instance 'quantity
                      :scale scale
                      :unit unit))))

  (set-dispatch-macro-character
   #\# #\q #'quantity-transformer-reader-macro)

  (defgeneric quantity (obj)
    (:documentation "Forms a quantity from more basic types, such as
  symbols and numbers.")
    ;; For default behavior:
    (:method (x)
      x))

  ;; defquantity defines a method on the quantity generic function

  ;;(eval-when (:compile-toplevel :load-toplevel)

  ;; Single-expansion version:
  ;; (defquantity quantity
  ;;     q q)

  ;; Smarter version:

  (defun unit->quantity (unit-list)
    "Converts raw units into a quantity."
    (cond
      ;; symbols and other non-lists
      ((not (listp unit-list))
       (quantity unit-list))
      (t
       (apply #'*
              (mapcar (lambda (u)
                        (if (listp u)
                            (apply #'expt u)
                            u))
                      (mklist unit-list))))))

  (defquantity quantity
      q
    ;; Process units
    (let* ((scale (quantity-scale q))
           (unit (quantity-unit q))
           (unit-quantity (unit->quantity unit))
           (new-unit (quantity-unit unit-quantity)))
      (if (equal unit new-unit)
          q
          (make-instance 'quantity
                         :scale (* scale (quantity-scale unit-quantity))
                         :unit (quantity-unit unit-quantity)))))

  (defquantity number
      n (make-instance 'quantity :scale n))

  ;; For base units
  (defquantity symbol
      s (make-instance 'quantity :scale 1 :unit s))

  ;; For err-nums:
  (defquantity err-num
      e (make-instance 'quantity :scale e :unit 1))

  (defmacro define-unit (unit-symbol quantity)
    "Defines a derived unit."
    (with-gensyms (x)
      `(defmethod quantity ((,x (eql ,unit-symbol)))
         ,quantity)))

;;; Generic math functions for quantities:

  (defgeneric quantity-if-necessary (q)
    (:documentation "Returns a quantity only if necessary."))

  (defmethod quantity-if-necessary ((q quantity))
    (with-slots (scale unit)
        q
      (if (or (null unit)
              (equal unit 1)
              (and (listp unit)
                   (or (and (length-equal unit 2)
                            (numberp (second unit))
                            (zerop (second unit)))
                       (and (length-equal unit 1)
                            (listp (first unit))
                            (numberp (second (first unit)))
                            (zerop (second (first unit)))))))
          scale
          q)))

  (defmethod quantity-if-necessary (x)
    x)

;;;; Methods on quantities are always done via the
;;;; define-quantity-method macro which defines all appropriate
;;;; methods for quantities and symbols as quantities.
;;;;
;;;; Method bodies are for quantity-only arguments; other combinations
;;;; are generated automatically making use of the quantity generic
;;;; function.
;;;;
;;;; At the moment, I do not have a centralized way to add a type to
;;;; the automatic quantity method generation, but the procedure for doing so is to simply

  ;; Note that addition & subtraction assume you know what you're doing,
  ;; no dimension checking.

  (define-quantity-method add (ql qr)
    (make-instance 'quantity
                   :scale (add (quantity-scale ql)
                               (quantity-scale qr))
                   :unit (quantity-unit ql)))

  (define-quantity-method sub (ql qr)
    (make-instance 'quantity
                   :scale (sub (quantity-scale ql)
                               (quantity-scale qr))
                   :unit (quantity-unit ql)))

  (define-quantity-method unary-sub (q)
    (make-instance 'quantity
                   :scale (unary-sub (quantity-scale q))
                   :unit (quantity-unit q)))

  (define-quantity-method mult (ql qr)
    (quantity-if-necessary
     (with-accessors ((scalel quantity-scale)
                      (unitl quantity-unit))
         ql
       (with-accessors ((scaler quantity-scale)
                        (unitr quantity-unit))
           qr
         (make-instance 'quantity
                        :scale (* scalel scaler)
                        :unit (unit-mult unitl unitr))))))

  (define-quantity-method unary-div (q)
    (with-slots (scale unit)
        q
      (make-instance 'quantity
                     :scale (unary-div scale)
                     :unit (unit-div 1 unit))))

  (define-quantity-method div (ql qr)
    (quantity-if-necessary
     (with-accessors ((scalel quantity-scale)
                      (unitl quantity-unit))
         ql
       (with-accessors ((scaler quantity-scale)
                        (unitr quantity-unit))
           qr
         (make-instance 'quantity
                        :scale (div scalel scaler)
                        :unit (unit-div unitl unitr))))))

  ;; Could provide the protected-div functions, but I'm lazy right now.

  ;; Note that expt treats x as a pure number, ignoring the unit for it
  ;; as a quantity.

  (define-quantity-method expt (q x)
    (with-slots (scale unit)
        q
      (let ((x (quantity-scale x)))
        (make-instance 'quantity
                       :scale (expt scale x)
                       :unit (unit-expt unit x)))))

  (define-quantity-method sqrt (q)
    (with-slots (scale unit)
        q
      (make-instance 'quantity
                     :scale (sqrt scale)
                     :unit unit)))

  (define-quantity-method exp (q)
    (with-slots (scale unit)
        q
      (make-instance 'quantity
                     :scale (exp scale)
                     :unit unit)))

  (define-quantity-method log (q)
    (with-slots (scale unit)
        q
      (make-instance 'quantity
                     :scale (log scale)
                     :unit unit)))

  (define-quantity-method sin (q)
    (with-slots (scale unit)
        q
      (make-instance 'quantity
                     :scale (sin scale)
                     :unit unit)))

  (define-quantity-method cos (q)
    (with-slots (scale unit)
        q
      (make-instance 'quantity
                     :scale (cos scale)
                     :unit unit)))

  (define-quantity-method tan (q)
    (with-slots (scale unit)
        q
      (make-instance 'quantity
                     :scale (tan scale)
                     :unit unit)))

  (define-quantity-method sinh (q)
    (with-slots (scale unit)
        q
      (make-instance 'quantity
                     :scale (sinh scale)
                     :unit unit)))

  (define-quantity-method cosh (q)
    (with-slots (scale unit)
        q
      (make-instance 'quantity
                     :scale (cosh scale)
                     :unit unit)))

  (define-quantity-method tanh (q)
    (with-slots (scale unit)
        q
      (make-instance 'quantity
                     :scale (tanh scale)
                     :unit unit)))

;;; Metric prefixes (e.g. mega, micro, kilo, ...)

  (defun ten-factor (x y)
    (* x
       (expt 10 y)))

  (defmacro define-metric-prefix (prefix-name exponent)
    `(defun ,prefix-name (x)
       (ten-factor x ,exponent)))

  (define-metric-prefix yotta  24)
  (define-metric-prefix zetta  21)
  (define-metric-prefix exa    18)
  (define-metric-prefix peta   15)
  (define-metric-prefix tera   12)
  (define-metric-prefix giga   9)
  (define-metric-prefix mega   6)
  (define-metric-prefix kilo   3)
  (define-metric-prefix hecto  2)
  (define-metric-prefix deca   1)
  (define-metric-prefix deci  -1)
  (define-metric-prefix centi -2)
  (define-metric-prefix milli -3)
  (define-metric-prefix micro -6)
  (define-metric-prefix nano  -9)
  (define-metric-prefix pico  -12)
  (define-metric-prefix femto -15)
  (define-metric-prefix atto  -18)
  (define-metric-prefix zepto -21)
  (define-metric-prefix yocto -24))
