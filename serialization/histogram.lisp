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
(in-package :serialization)

(defparameter *histogram-data-path* "data")

(defparameter *histogram-bin-spec-path* "bin-specs")

(defun ->double-float (x)
  (float x 0d0))

(defun write-histogram (histogram file hdf-path)
  "Writes histogram to file assuming it contains double-float data as
bin centers and integers/fixnums as bin count values; this may be
fixed in the future to allow for things like err-num bin values; in
the mean time one can create two histograms, one with the bin count
and the other with the error bars as the bin values.

Note that this function assumes that either all the dimensions have
names or none of them do."
  (hdf-mkgroup file hdf-path)
  (flet ((subpath (path)
           (concatenate 'string
                        hdf-path
                        "/"
                        path)))
    (let* ((data-names-specs
            (cons (cons "count" :int)
                  (loop
                     for i from 0
                     for n in (hist-dim-names histogram)
                     collect (cons (if n
                                       n
                                       (with-output-to-string (s)
                                         (format s "x~a" i)))
                                   :double))))
           (data (hist-bin-values histogram))
           (data-table-path
            (subpath *histogram-data-path*))
           (data-table (create-hdf-table
                        file data-table-path data-names-specs))
           (data-field-symbols (table-field-symbols data-table))
           (bin-spec-plists
            (let ((result (bin-spec-plists histogram)))
              (loop
                 for plist in result
                 for i from 0
                 do (when (not (getf plist :name))
                      (setf (getf plist :name)
                            (with-output-to-string (s)
                              (format s "x~a" i)))))
              result))
           (bin-spec-table-path (subpath *histogram-bin-spec-path*))
           (max-string-length
            (loop
               for plist in bin-spec-plists
               maximizing (length (getf plist :name))))
           (bin-spec-names-specs
            (list (cons "name" (list :array :char max-string-length))
                  (cons "name-length" :int)
                  (cons "nbins" :int)
                  (cons "low" :double)
                  (cons "high" :double)))
           (bin-spec-table (create-hdf-table
                            file bin-spec-table-path
                            bin-spec-names-specs)))
      ;; write data table
      (loop
         for datum in data
         do (progn
              (loop
                 for i from 0
                 for field in datum
                 for sym in data-field-symbols
                 do (table-set-field data-table sym
                                     (if (zerop i)
                                         field
                                         (->double-float field))))
              (table-commit-row data-table)))
      (table-close data-table)
      ;; write bin-spec table
      ;; fix plists
      (loop
         for plist in bin-spec-plists
         for i from 0
         do (when (not (getf plist :name))
              (setf (getf plist :name)
                    (with-output-to-string (s)
                      (format s "x~a" i)))))
      (loop
         for plist in bin-spec-plists
         do (progn
              (table-push-fields bin-spec-table
                (name (getf plist :name))
                (name-length (length (getf plist :name)))
                (nbins (getf plist :nbins))
                (low (->double-float (getf plist :low)))
                (high (->double-float (getf plist :high))))))
      (table-close bin-spec-table))))

(defun read-histogram (file hdf-path &optional (type :sparse))
  "Reads a histogram from an hdf-table with file and path.

type can be either :contiguous or :sparse for contiguous-histogram and
sparse-histogram respectively."
  (flet ((subpath (path)
           (concatenate 'string
                        hdf-path
                        "/"
                        path)))
    (let* ((bin-spec-table
            (open-hdf-table file
                            (subpath *histogram-bin-spec-path*)))
           (bin-spec-table-field-names
            (list "name" "name-length" "low" "high" "nbins"))
           (bin-spec-plists
            (let ((result ()))
              (table-reduce bin-spec-table
                            bin-spec-table-field-names
                            (lambda (state name name-length low high nbins)
                              (push (list :name (char-vector->string name name-length)
                                          :low low
                                          :high high
                                          :nbins nbins)
                                    result)))
              (nreverse result)))
           (histogram
            (cond
              ((equal type :contiguous)
               (make-contiguous-hist bin-spec-plists))
              ((equal type :sparse)
               (make-sparse-hist bin-spec-plists))
              (t (error "Must specify :contiguous or :sparse for
              type."))))
           (data-table
            (open-hdf-table file
                            (subpath *histogram-data-path*)))
           (data-table-field-names
            (table-field-names data-table))
           (names-specs nil))
      (table-reduce data-table
                    data-table-field-names
                    (lambda (state count &rest xs)
                      (hist-insert histogram xs count)))
      (table-close bin-spec-table)
      (table-close data-table)
      histogram)))
