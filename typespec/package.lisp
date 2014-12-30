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

(defpackage #:cl-ana.typespec
  (:use :cl
        :alexandria
	:cffi
        :cl-ana.int-char
	:cl-ana.list-utils
	:cl-ana.string-utils
        :cl-ana.symbol-utils
        :cl-ana.tensor
	:cl-ana.memoization)
  (:export :typespec->cffi-type
           :typespec-foreign-alloc
           ;; compound typespec utilities:
	   :typespec-compound-p
           :typespec-compound-field-alist
           :typespec-compound-field-names
           :typespec-compound-field-specs
           ;; array typespec utilities:
	   :typespec-array-p
           :typespec-array-element-type
           :typespec-array-dim-list
           :typespec-array-rank
           :typespec-array-size
           ;; A function for setting the values of a foreign object
           ;; recursively; currently this is not directly provided by
           ;; CFFI
           :typespec->lisp-to-c
           ;; Returns a function which, when applied to a C-pointer,
           ;; returns a lisp object corresponding to the C object
           :typespec->c-to-lisp
           ;; I may decide to use this (with changes) for providing
           ;; automatic flattening of typespecs so that nested array
           ;; types can be used as well as directly specifying the
           ;; rank of a multi-array:
	   :typespec-flatten-arrays
           ;; Function for handling strings stored as character arrays:
           :char-vector->string))
