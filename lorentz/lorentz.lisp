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

(in-package :cl-ana.lorentz)

;;; This library of lorentz transformation-related utilities assumes
;;; dogmatically that
;;;
;;; 1. Time component is the zeroth element.
;;; 2. We are using the (+,-,-,...) Minkowski space metric.
;;;
;;; This implementation does lack a general lorentz transformation as
;;; it only provides the boost facilities.  But: you can do any
;;; general lorentz transformation by using a boost followed by a
;;; rotation.
;;;
;;; The library also provides the useful reader macro for lorentz
;;; vectors: #L(...), which accepts exactly 4 values inside of the
;;; parentheses which will become the time, x, y, and z components
;;; respectively.

(defun minkowski-dot (left-vector right-vector)
  "Computes the inner product using the Minkowski metric; only
requires the tensor-ref function to be defined and that the vectors each
have exactly 4 elements each."
  (flet ((variance-flip-factor (i)
	   (if (zerop i)
	       1
	       -1)))
    (let ((left-length (first (tensor-dimensions left-vector)))
	  (right-length (first (tensor-dimensions right-vector))))
      (loop
	 for i from 0 below left-length
	 for j from 0 below right-length
	 summing (* (variance-flip-factor i)
		    (tensor-ref left-vector i)
		    (tensor-ref right-vector j))))))

(defun minkowski-norm (vector)
  (sqrt (minkowski-norm2 vector)))

(defun minkowski-norm2 (vector)
  (minkowski-dot vector vector))

(defun make-lorentz-boost (beta-vector)
  "Construct lorentz boost matrix from beta vector"
  (let* ((lorentz-boost (make-matrix 4 4))
	 (beta2 (euclidean-norm2 beta-vector))
	 (gamma (gamma-from-beta2 beta2)))
    (if (zerop beta2)
        (list (list 1 0 0 0)
              (list 0 1 0 0)
              (list 0 0 1 0)
              (list 0 0 0 1))
        ;; matrix is symmetric, so we can fill one side and then set the
        ;; other to be the same main diagonal: top-left corner:
        (progn
          (setf (tensor-ref lorentz-boost 0 0)
                gamma)
          ;; spatial diagonal:
          (loop
             for i from 1 to 3
             do (setf (tensor-ref lorentz-boost i i)
                      (+ 1
                         (* (- gamma 1)
                            (/ (expt (tensor-ref beta-vector (1- i)) 2)
                               beta2)))))
          ;; spatial off-diagonal:
          (loop
             for i from 1 to 2
             do (loop
                   for j from (1+ i) to 3
                   do (progn
                        (setf (tensor-ref lorentz-boost i j)
                              (* (- gamma 1)
                                 (tensor-ref beta-vector (1- i))
                                 (tensor-ref beta-vector (1- j))
                                 (/ beta2)))
                        (setf (tensor-ref lorentz-boost j i)
                              (tensor-ref lorentz-boost i j)))))
          ;; rest of first row & first column
          (loop
             for i from 1 to 3
             do (progn
                  (setf (tensor-ref lorentz-boost 0 i)
                        (- (* gamma
                              (tensor-ref beta-vector (1- i)))))
                  (setf (tensor-ref lorentz-boost i 0)
                        (tensor-ref lorentz-boost 0 i))))
          lorentz-boost))))

;;; miscellaneous physics functions:

(defun gamma (beta)
  "Returns gamma factor from beta"
  (gamma-from-beta2 (expt beta 2)))

(defun gamma-from-beta2 (beta2)
  "Computes gamma from beta^2, for efficiency purposes"
  ;; protection against infinities:
  (when (not (= beta2 1))
    (/ (sqrt (- 1
                beta2)))))

(defun lorentz-vector-spatial (vector)
  "Returns spatial part of the lorentz-vector"
  (let* ((length (first (tensor-dimensions vector)))
	 (spatial-part (make-vector (1- length))))
    (loop
       for i from 1 below length
       do (setf (tensor-ref spatial-part (1- i))
		(tensor-ref vector i)))
    spatial-part))

(defun four-momentum-beta-vector (four-momentum)
  "Returns the beta vector from the four-momentum.  Assumes that your
units are chosen such that c is effectively 1 (e.g. GeV/c^2 for mass,
GeV/c for momentum, etc.)"
  (let ((momentum-vector (lorentz-vector-spatial four-momentum))
	(energy (tensor-ref four-momentum 0)))
    (div momentum-vector energy)))

;; Angles for lorentz spatial components:

(defun lorentz-phi (lorentz-vector)
  (phi (lorentz-vector-spatial lorentz-vector)))

(defun lorentz-theta (lorentz-vector)
  (theta (lorentz-vector-spatial lorentz-vector)))
