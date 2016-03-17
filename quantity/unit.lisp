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

;;;; Units
;;; Units are, in the context of this library, restricted to base
;;; units, i.e. those directly corresponding to the base dimensions.
;;; They are directly represented via keyword symbols; properties of
;;; units are stored in global tables.

;;; Default units are S.I. units

(defvar *default-units* (make-hash-table :test 'equal)
  "Maps from dimension to the default unit for that dimension.")

(defun set-default-unit (dimension unit)
  "Sets the default unit for a dimension."
  (setf (gethash dimension *default-units* )
        unit))

;;; Units utilities

(defun unit-symbol (unit)
  (if (symbolp unit)
      unit
      (unit-symbol
       (first unit))))

(defun unit-string (unit)
  (string (unit-symbol unit)))

(defun unit-first< (unit1 unit2)
  (string< (unit-string unit1)
           (unit-string unit2)))

(defun unit-first= (unit1 unit2)
  (string= (unit-string unit1)
           (unit-string unit2)))

(defun unit-standard-order (unit)
  (if (not (listp unit))
      unit
      (if (and (length-equal unit 2)
               (numberp (second unit)))
          (list unit)
          (let* ((copy (copy-list unit)))
            (sort copy #'string<
                  :key #'unit-string)))))

(defun ensure-explicit-exponent (u)
  "Ensures that a unit is expressed as a list."
  (if (symbolp u)
      (list u 1)
      u))

(defun unit-compound-form (u)
  "Ensures that a unit is expressed as a compound unit list."
  (if (symbolp u)
      (list u)
      (if (and (length-equal u 2)
               (numberp (second u)))
          (list u)
          u)))

(defun unit-standard-form (u)
  "Ensures that a unit is in compound form with each component unit
being expressed with explicit exponent."
  (if (equal u 1)
      (list (list 1 0))
      (mapcar #'ensure-explicit-exponent
              (unit-compound-form u))))

(defun un-singleton-zero (lst)
  (remove-if #'null
             (mapcar (lambda (x)
                       (if (and (listp x)
                                (numberp (second x)))
                           (cond
                             ((= (second x) 1)
                              (first x))
                             ((zerop (second x))
                              nil)
                             (t x))
                           x))
                     lst)))

(defun one-if-null (x)
  (if (null x)
      1
      x))

(defun unwrap-single (l)
  (if (single l)
      (car l)
      l))

(defun unit-simplify (u)
  "Expects u to be in standard form"
  (unwrap-single
   (one-if-null
    (un-singleton-zero u))))

(defun unit-mult (unit1 unit2)
  (labels ((ucoef (u)
             (if (symbolp u)
                 1
                 (second u)))
           (umult (u1 u2)
             (+ (ucoef u1)
                (ucoef u2))))
    (cond
      ((equal unit1 1)
       unit2)
      ((equal unit2 1)
       unit1)
      (t
       (when (not (unit-first< unit1 unit2))
         (rotatef unit1 unit2))
       (let ((ulst1 (unit-standard-form unit1))
             (ulst2 (unit-standard-form unit2))
             (result ()))
         (unit-simplify
          (do ()
              ((or (null ulst1)
                   (null ulst2))
               (append result ulst1 ulst2))
            (let ((u1 (first ulst1))
                  (u2 (first ulst2)))
              (cond
                ((string< (unit-string u1)
                          (unit-string u2))
                 (push u1 result)
                 (setf ulst1 (rest ulst1)))
                ((string= (unit-string u1)
                          (unit-string u2))
                 (push (list (unit-symbol u1)
                             (umult u1 u2))
                       result)
                 (setf ulst1 (rest ulst1))
                 (setf ulst2 (rest ulst2)))
                (t
                 (push u2 result)
                 (setf ulst2 (rest ulst2))))))))))))

(defun unit-div (unit1 unit2)
  (labels ((negate-unit-coefs (u)
             (cond
               ((equal u 1)
                1)
               ((symbolp u)
                (list u -1))
               ((and (length-equal u 2)
                     (numberp (second u)))
                (list (first u)
                      (- (second u))))
               (t
                (mapcar #'negate-unit-coefs u)))))
    (unit-mult unit1
               (unit-simplify
                (negate-unit-coefs
                 (unit-standard-form unit2))))))

(defun unit-expt (unit power)
  (unit-simplify
   (let ((unit (unit-standard-form unit)))
     (mapcar (lambda (u)
               (list (first u)
                     (* (second u)
                        power)))
             unit))))
