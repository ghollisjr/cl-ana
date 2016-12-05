;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2016 Gary Hollis
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

;;;; This project should not run properly, since there is a res form
;;;; in an lfield.  This functionality used to be supported, but was
;;;; very difficult to implement accurately and efficiently.  Maybe
;;;; someone will add it back in the future, but there is no loss in
;;;; functionality, since creating a logical table and loading a
;;;; result as an init is a practical equivalent.

(defproject lfields-dep-test
    ;; change this to something that makes sense
    "/home/ghollisjr/lfields-dep-test"
  (list #'progresstrans #'macrotrans #'tabletrans)
  (fixed-cache 20))

(defres src
  (wrap-for-reuse
   (open-plist-table (list (list :|x| 1)
                           (list :|x| 2)
                           (list :|x| 3)))))

(defres (mean src x)
  (dotab (res src)
      ((sum 0)
       (count 0))
      (/ sum count)
    (incf sum (field |x|))
    (incf count)))

;; New table to support logical fields based on previous calculations
(defres proc
  (ltab (res src) ()
    (push-fields)))

(deflfields proc
    ((delta-x (- (field |x|)
                 (res (mean src x))))))

;; should be 0
(defres (mean proc delta-x)
  (dotab (res proc)
      ((sum 0)
       (count 0))
      (/ sum count)
    (incf sum (field delta-x))
    (incf count)))
