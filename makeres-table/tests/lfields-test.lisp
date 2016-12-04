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

(require 'cl-ana)

(in-package :cl-ana)

(defproject lfields-test
    "/home/ghollisjr/lfields-test"
  (list #'macrotrans #'tabletrans)
  (fixed-cache 20))

(defres src
  (wrap-for-reuse
   (open-plist-table (list (list :|x| 1)
                           (list :|x| 2)
                           (list :|x| 3)))))

(logres-ignore src)

(deflfields src
    ((|y| (+ (field |x|)
             1))))

(defres tab
  (tab (res src)
      ()
      (hdf-opener (work-path "lfields-test.h5")
                  (list (cons "x" :int)
                        (cons "y" :int)))
    (print (field |x|))
    (print (field |y|))
    (push-fields
     (|x| (field |x|))
     (|y| (field |y|)))))

(deflfields tab
    ((|z| (+ (field |x|)
             (field |y|)))))

(defres (mean tab z)
  (dotab (res tab)
      ((sum 0)
       (count 0))
      (/ sum count)
    (incf sum (field |z|))
    (incf count)))
