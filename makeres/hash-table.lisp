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

(defmethod printable ((obj hash-table))
  nil)

(defmethod save-object ((ht hash-table) path)
  (let* ((savedir
          (make-pathname
           :directory (namestring path)))
         (indexpath (merge-pathnames "index"
                                     savedir)))
    (ensure-directories-exist savedir)
    (let ((index-alist nil))
      (loop
         for k being the hash-keys in ht
         for v being the hash-values in ht
         for i from 0
         do ;; save content:
           (push (cons (list i (target-type k))
                       (list i (target-type v)))
                 index-alist)
           (save-object k
                        (merge-pathnames (mkstr "k" i)
                                         savedir))
           (save-object v
                        (merge-pathnames (mkstr "v" i)
                                         savedir)))
      (with-open-file (file indexpath
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
        (format file "~s~%" (nreverse index-alist))
        (format file "~s~%" (hash-table-test ht))))))

(defmethod load-object ((type (eql 'hash-table)) path)
  (let* ((loaddir
          (make-pathname
           :directory (namestring path)))
         (indexpath (merge-pathnames "index" loaddir)))
    (let ((index-alist nil)
          (test nil))
      (with-open-file (file indexpath
                            :direction :input
                            :if-does-not-exist :error)
        (setf index-alist (read file))
        (setf test (read file)))
      (map->hash-table
       (mapcar (lambda (cons)
                 (destructuring-bind ((kid ktype) . (vid vtype))
                     cons
                   (cons (load-object ktype
                                      (merge-pathnames
                                       (mkstr "k" kid)
                                       loaddir))
                         (load-object vtype
                                      (merge-pathnames
                                       (mkstr "v" vid)
                                       loaddir)))))
               index-alist)
       test))))
