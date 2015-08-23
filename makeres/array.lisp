;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013-2015 Gary Hollis
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

(in-package :cl-ana.makeres)

;; fix in the future
(defmethod printable ((vector array))
  nil)

(defmethod save-object ((vector array) path)
  (let* ((savedir
          (make-pathname
           :directory (namestring path)))
         (indexpath (merge-pathnames "index"
                                     savedir)))
    (ensure-directories-exist savedir)
    (let ((index-list
           (loop
              for elt across vector
              for i from 0
              collect
                (let ((type (target-type elt)))
                  (save-object elt
                               (merge-pathnames (mkstr i)
                                                savedir))
                  (list i type)))))
      (with-open-file (file indexpath
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
        (format file
                "~a~%"
                index-list)))))

(defmethod load-object ((type (eql 'array)) path)
  (let* ((loaddir
          (make-pathname
           :directory (namestring path)))
         (indexpath (merge-pathnames "index" loaddir)))
    (let ((index-list nil))
      (with-open-file (file indexpath
                            :direction :input
                            :if-does-not-exist :error)
        (setf index-list
              (read file)))
      (map 'vector
           (lambda (elt)
             (destructuring-bind (elt-id elt-type)
                 elt
               (load-object elt-type
                            (merge-pathnames
                             (mkstr elt-id)
                             loaddir))))
           index-list))))
