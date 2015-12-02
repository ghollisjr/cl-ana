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

(in-package :cl-ana.string-utils)

(defun lispify (symbol-or-string)
  "Converts symbol or string into a string and replaces all spaces and
  underscores with -, and convert to uppercase."
  (string-upcase (map 'string
		      (lambda (c)
                        (case c
                          (#\Space     #\-)
                          (#\_         #\-)
                          (otherwise   c)))
		      (string symbol-or-string))))

(defun string-append (&rest strings)
  (apply #'concatenate 'string strings))

;; Useful for getting the words out of a string:
(defun words (string)
  "Returns the read words (symbols, numbers, etc.) contained in a
string"
  (with-input-from-string (s string)
    (do ((w (read s nil nil) (read s nil nil))
         (res nil (cons w res)))
        ((null w) (nreverse res)))))

;;;; From let-over-lambda:

(defun mkstr (&rest args)
  (let ((*print-pretty* nil))
    (with-output-to-string (s)
      (dolist (a args)
        (princ a s)))))
