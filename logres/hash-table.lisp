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

(in-package :cl-ana.logres)

;; For now, hash-tables are converted into alists, but in the future
;; it would be nice if all container objects would use their storage
;; file as an index, allowing e.g. lists of histograms to be stored
;; (histograms are stored via HDF5, not text files).

(defmethod save-target (lid (ht hash-table) path)
  (let ((savedir
         (make-pathname
          :directory (pathname-directory path))))

    (let ((index-alist nil))
      (loop
         for k being the hash-keys in ht
         for v being the hash-values in ht
         do (let ((kid (next-log-id))
                  (vid (next-log-id)))
              ;; log sublids:
              (symbol-macrolet ((sublids
                                 (gethash lid
                                          (gethash *project-id*
                                                   *proj->lid->sublids*))))
                (push kid
                      sublids)
                (push vid sublids))
              ;; save content:
              (push (cons (list kid (target-type k))
                          (list vid (target-type v)))
                    index-alist)
              (save-target kid
                           k
                           (merge-pathnames (mkstr kid)
                                            savedir))
              (save-target vid
                           v
                           (merge-pathnames (mkstr vid)
                                            savedir))))
      (with-open-file (file path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
        (format file "~s~%" (nreverse index-alist))
        (format file "~s~%" (hash-table-test ht))))))

(defmethod load-target ((type (eql 'hash-table)) path)
  (let ((loaddir
         (make-pathname
          :directory (pathname-directory path))))
    (let ((index-alist nil)
          (test nil))
      (with-open-file (file path
                            :direction :input
                            :if-does-not-exist :error)
        (setf index-alist (read file))
        (setf test (read file)))
      (map->hash-table
       (mapcar (lambda (cons)
                 (destructuring-bind ((kid ktype) . (vid vtype))
                     cons
                   (cons (load-target ktype
                                      (merge-pathnames
                                       (mkstr kid)
                                       loaddir))
                         (load-target vtype
                                      (merge-pathnames
                                       (mkstr vid)
                                       loaddir)))))
               index-alist)
       test))))
