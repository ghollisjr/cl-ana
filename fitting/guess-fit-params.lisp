;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2023 Gary Hollis
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

(in-package :cl-ana.fitting)

;;; Generic function for guessing parameters.  #'fit has been updated
;;; to allow NIL init-params values, which trigger guess-fit-params.
(defgeneric guess-fit-params (data fn &key &allow-other-keys)
  (:documentation "Define a method for this function in order to allow
  parameter guessing for #'fit to automatically guess initial fit
  parameters."))

;;; Basic guesses for commonly used fit functions:
;; polynomials (:n controls number of parameters)
(defmethod guess-fit-params (data (fn (eql #'polynomial))
                             &key (n 2) &allow-other-keys)
  (loop repeat n collecting 0d0))

(defmethod guess-fit-params (data (fn (eql #'gaussian))
                             &key &allow-other-keys)
  (let* ((alist (map->alist data)))
    (multiple-value-bind (peak mean)
        (loop
          with peak = (cons nil nil)
          for cons in alist
          summing (float (* (car cons) (cdr cons)) 0d0) into meansum
          summing (float (cdr cons) 0d0) into meancount
          when (or (null (cdr peak))
                   (<= (cdr peak) (cdr cons)))
            do (setf peak cons)
          finally (return (values peak (protected-/ meansum meancount))))
      (let* ((sigma
               (sqrt
                (loop
                  for cons in alist
                  summing (* (expt (- (car cons) mean) 2) (cdr cons)) into sum
                  summing (cdr cons) into count
                  finally (return (protected-/ sum count))))))
        (list (gauss-amp (cdr peak) sigma) mean sigma)))))
