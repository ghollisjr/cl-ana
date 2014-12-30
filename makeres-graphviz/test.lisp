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

(require 'cl-ana.makeres-graphviz)

(in-package :cl-ana.makeres-graphviz)

(in-project makeres-graphviz)

(defres src
  (list 1 2 3 4 5))

(defres squares
  (mapcar (lambda (x)
            (expt x 2))
          (res src)))

(defres mean
  (mean (res src)))

(defres mean-squares
  (mean (res squares)))

(defres variance
  (- (res mean-squares)
     (expt (res mean)
           2)))

(defres standard-deviation
  (sqrt (res variance)))
