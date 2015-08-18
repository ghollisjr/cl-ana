;;;; makeres-macro is a Common Lisp make-like tool for computations.
;;;; Copyright 2014 Gary Hollis
;;;; 
;;;; This file is part of makeres-macro.
;;;; 
;;;; makeres-macro is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; makeres-macro is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with makeres-macro.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis via email at
;;;; ghollisjr@gmail.com

(require 'cl-ana.makeres-macro)

(in-package cl-ana.makeres-macro)

(in-project makeres-macro)

(settrans (list #'macrotrans) :op :set)

(define-res-macro tagged-res (&rest tags)
  `(list ,@(loop
              for m in (loop
                          for id being the hash-keys in (target-table)
                          when (every (lambda (tag)
                                        (member tag id :test #'eq))
                                      tags)
                          collect id)
              append (copy-list `((res ,m))))))

(defres (x) 'x)

(defres (y) 'y)

(defres (test)
  (tagged-res x))
