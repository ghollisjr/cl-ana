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

(defpackage #:cl-ana.macro-utils 
  (:use #:cl
        #:split-sequence
        #:alexandria
        #:cl-ana.list-utils
        #:cl-ana.string-utils
        #:cl-ana.symbol-utils)
  (:export :defplural
           :inrange
           :case-equal
	   :cond-setf
	   :print-eval
           :with-default-args
           :when-keywords
           :defun-with-setf
           :abbrev
           :abbrevs
           :funcall-bind
           :map-bind
           :symb
           :dbind
           :mvbind
           :mvsetq
           :fvbind
           ;; Looping construct for polling:
           :poll
           ;; anaphoric macros:
           :it
           :self
           :alambda
           :aif
           :awhen
           ;; macro for timing processes (returning time)
           :time-proc
           ;; handling lambda lists:
           :lambda-list-call-form
           ;; Suppress output:
           :suppress-output
           ;; lambda especially for keywords
           :klambda
           ;; once-only let macro
           :olet
           ;; dlambda (dispatching lambda) from let over lambda
           :dlambda))
