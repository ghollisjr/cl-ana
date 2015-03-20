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

(in-package :cl-ana.file-utils)

(defun read-lines-from-file (file)
  (loop
     for line = (read-line file nil 'eof)
     until (equal line 'eof)
     collecting line))

(defun read-lines-from-pathname (pathname)
  (with-open-file (file pathname
                        :direction :input
                        :if-does-not-exist :error)
    (read-lines-from-file file)))

(defun read-fields-from-file (file)
  (loop
     for line = (read-line file nil 'eof)
     until (equal line 'eof)
     collecting (line->fields line)))

(defun line->fields (line)
  (labels ((rec (stream acc)
             (handler-case (rec stream
                                (cons (read stream)
                                      acc))
               (error nil (nreverse acc)))))
    (with-input-from-string (s line)
      (rec s nil))))

(defun read-fields-from-pathname (pathname)
  (with-open-file (infile pathname
                          :direction :input
                          :if-does-not-exist :error)
    (read-fields-from-file infile)))
