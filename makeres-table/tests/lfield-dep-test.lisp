;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013-2015 Gary Hollis
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

(require 'cl-ana)

(in-package :cl-ana)

(defproject lfield-dep-test
    "/home/ghollisjr/test/lfield-dep-test"
  (list #'macrotrans #'tabletrans #'progresstrans)
  (fixed-cache 5))

;; To perform this test, redefine a and call
;;
;; (makeres-propogate!) and then
;; (makeres)
;;
;; to see if b gets recalculated

(defres a
  5)

(defres src
  (srctab (plist-opener '((:x 1)
                          (:x 2)
                          (:x 3)))))

(deflfields src
    ((z
      (+ (field x) (res a)))))

(defres b
  (dotab (res src)
      ((z-sum 0))
      z-sum
    (incf z-sum (field z))))
