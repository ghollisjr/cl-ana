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

(in-package :cl-ana.pathname-utils)

(defun pathname-absolute-p (pathname-or-string)
  (let* ((pathname (pathname pathname-or-string))
         (directory (pathname-directory pathname)))
    (when directory
      (equal (first directory)
             :absolute))))

(defun pathname-relative-p (pathname-or-string)
  (not (pathname-absolute-p pathname-or-string)))

(defun ->absolute-pathname (pathname-or-string)
  (let ((pathname (pathname pathname-or-string)))
    (if (pathname-relative-p pathname)
        ;; handle relative
        (merge-pathnames pathname)
        pathname)))

(defun directory-pathname-p (pathname-or-string)
  "Returns t iff pathname-or-string refers to a directory"
  (string= (file-namestring (pathname pathname-or-string))
           ""))

(defun mkdirpath (pathname-or-string)
  "Returns a path which always refers to a directory (platform
independent)"
  (let ((pn (merge-pathnames pathname-or-string)))
    (if (directory-pathname-p pn)
        pn
        (let ((dirname (directory-namestring pn))
              (filename (file-namestring pn)))
          (make-pathname :directory
                         (list :absolute
                               dirname
                               filename))))))

(defun basename (pathname)
  "Returns basename of pathname; pathname-name strips the extension
while this utility function preserves it."
  (let ((pathname (namestring pathname)))
    (enough-namestring
     pathname
     (make-pathname :directory
                    (pathname-directory pathname)))))
