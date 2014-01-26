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
;;;; gnuplot-i-cffi.lisp

;;;; This software uses the gnuplot_i library written by N. Devillard.

(in-package :gnuplot-i-cffi)

(define-foreign-library gnuplot-i-cffi
  (t (:default "libgnuploti")))

(use-foreign-library gnuplot-i-cffi)

;; To start a gnuplot session:
(defcfun "gnuplot_init" :pointer)

;; This function acts like printf, but I'm not sure I'll use these
;; capabilities.  It feeds a command string into the gnuplot session
;; you specify; this provides complete access to gnuplot.
(defcfun "gnuplot_cmd" :void
  (session-handle :pointer) ; as returned by gnuplot_init
  (command :string)
  &rest)

;; To close the gnuplot session (important for cleaning up temp files
;; in /tmp etc.):
(defcfun "gnuplot_close" :void
  (session-handle :pointer))
