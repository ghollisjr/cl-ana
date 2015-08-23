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

(defun printable-cons (obj)
  "Returns T if obj is a cons structure which can be printed"
  #+sbcl
  (let ((standard-method
         (first
          (sb-mop:compute-applicable-methods #'save-object
                                             (list nil "")))))
    (or (equal (first
                (sb-mop:compute-applicable-methods
                 #'save-object (list obj "")))
               standard-method)
        (and (consp obj)
             (printable-cons (car obj))
             (printable-cons (cdr obj)))))
  ;; Default is no printable objects
  #-(or sbcl)
  nil)

(defmethod printable ((obj cons))
  (or (null obj)
      (and (printable (car obj))
           (printable (cdr obj)))))

(defmethod save-object ((cell cons) path)
  (if (printable cell)
      (with-open-file (file path
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
        (format file "~s~%" cell))
      (let* ((savedir
              (make-pathname
               :directory (namestring path)))
             (indexpath (merge-pathnames "index" savedir)))
        (ensure-directories-exist savedir)
        (let ((car-type (target-type (car cell)))
              (cdr-type (target-type (cdr cell))))
          (with-open-file (file indexpath
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
            (format file "~a~%"
                    (cons car-type
                          cdr-type)))
          (save-object (car cell)
                       (merge-pathnames "car"
                                        savedir))
          (save-object (cdr cell)
                       (merge-pathnames "cdr"
                                        savedir))))))

(defmethod load-object ((type (eql 'cons)) path)
  (if (cl-fad:directory-exists-p path)
      ;; general method
      (let* ((loaddir
              (make-pathname
               :directory (namestring path)))
             (indexpath (merge-pathnames "index" loaddir)))
        (let ((index-cons nil))
          (with-open-file (file indexpath
                                :direction :input
                                :if-does-not-exist :error)
            (setf index-cons (read file)))
          ;; (print index-cons)
          (destructuring-bind (car-type . cdr-type)
              index-cons
            ;; (print car-type)
            ;; (print cdr-type)
            (cons (load-object car-type
                               (merge-pathnames
                                "car"
                                loaddir))
                  (load-object cdr-type
                               (merge-pathnames
                                "cdr"
                                loaddir))))))
      ;; special case for printable cons structures
      (with-open-file (file path
                            :direction :input)
        (read file))))
