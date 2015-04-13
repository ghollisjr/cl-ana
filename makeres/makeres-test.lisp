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

;;;; Demo of language:

;;; Must use/select a project before using makeres

;; Select project
(in-project test)
;; project ID can be any lisp form

;; Select transformations (simply omit this expression if you don't
;; want any):
(settrans (list #'lrestrans))
;; We're using the logical result transformation

;; Define parameters for project
(defpars
    ((source (list 1 2 3 4 5 6 7))
     (scale 1)))

;; Each parameter form will be used in a keyword lambda-list, so you
;; can provide default values if you like or just use a symbol if
;; default should be nil.

;;; Results to be computed are defined via defres.  Arguments are an
;;; id (any lisp form) and a body of expressions to be evaluated to
;;; yield the value.
;;;
;;; Note that the transformation pipeline can give meaning to
;;; otherwise invalid expressions, making it possible to define DSLs
;;; for use with makeres which would be unwieldy otherwise.

;; Notice that (par source) is used to refer to the parameter "source"
(defres filtered
  (print 'filtered)
  (remove-if (lambda (x)
               (< x 5))
             (par source)))

;; Notice that (res filtered) is used to refer to the result target
;; "filtered"
(defres squared
  (print 'squared)
  (mapcar (lambda (x)
            (* x x))
          (res filtered)))

;; And this combines par and res.  Also notice that target ids can be
;; any form, not just symbols
(defres (sum scaled)
  (print '(sum scaled))
  (* (par scale)
     (+ (res filtered)
        (res squared))))

;; To demonstrate the logical result transformation, we'll define a
;; logical result target:
(defres lres-test
  (lres (print 'lres-test)))

;;; execute (makeres) to test.  makeres accepts keyword arguments for
;;; whatever have been defined via defpars.
(makeres)
;; or with other arguments:
(makeres :source (list 9 10))
(makeres :source (list 9 15)
         :scale -1)

;;; After you've run whatever computations you're interested in, you
;;; can examine the results with the res macro:

(print (res filtered))
(print (res squared))
(print (res (sum scaled)))

;;; You can also examine what the last parameter values were:

(print (par source))
(print (par scale))
