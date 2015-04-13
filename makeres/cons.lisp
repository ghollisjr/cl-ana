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

(defmethod save-target (lid (cell cons) path)
  (let ((savedir
         (make-pathname
          :directory (pathname-directory path))))

    (let ((car-id (next-log-id))
          (car-type (target-type (car cell)))
          (cdr-id (next-log-id))
          (cdr-type (target-type (cdr cell))))
      (symbol-macrolet ((sublids
                         (gethash lid
                                  (gethash *project-id*
                                           *proj->lid->sublids*))))
        (push car-id sublids)
        (push cdr-id sublids)
        (with-open-file (file path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
          (format file "~a~%"
                  (cons (list car-id car-type)
                        (list cdr-id cdr-type))))
        (save-target car-id
                     (car cell)
                     (merge-pathnames (mkstr car-id)
                                      savedir))
        (save-target cdr-id
                     (cdr cell)
                     (merge-pathnames (mkstr cdr-id)
                                      savedir))))))

(defmethod load-target ((type (eql 'cons)) path)
  (let ((loaddir
         (make-pathname
          :directory (pathname-directory path))))
    (let ((index-cons nil))
      (with-open-file (file path
                            :direction :input
                            :if-does-not-exist :error)
        (setf index-cons (read file)))
      (destructuring-bind ((car-id car-type) . (cdr-id cdr-type))
          index-cons
        (cons (load-target car-type
                           (merge-pathnames
                            (mkstr car-id)
                            loaddir))
              (load-target cdr-type
                           (merge-pathnames
                            (mkstr cdr-id)
                            loaddir)))))))
