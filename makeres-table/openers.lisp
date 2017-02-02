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

(in-package :cl-ana.makeres-table)

;;;; This file defines openers for use with tab for various tables
;;;; provided by cl-ana.
;;;;
;;;; Openers are functions which return closures to automate the
;;;; reading and writing of tables.
;;;;
;;;; The function returned by an opener should accept a single keyword
;;;; argument used as follows:
;;;;
;;;; :read requests that a table object be returned ready for reading
;;;; whatever was last written to the table object owned by the
;;;; opener, appropriately storing/closing any resources necessary.
;;;;
;;;; :write requests that a table object be returned ready for
;;;; writing, erasing whatever results were previously in the table.

;; hdf-table & hdf-table-chain:
(defun hdf-opener (path fields-specs
                   &key
                     buffer-size
                     (group "/table"))
  "Returns a closure which, when given a single keyword argument,
  returns an hdf-table ready for reading/writing, automatically
  managing file access and calling table-close when necessary."
  (let ((file nil)
        (table nil))
    (lambda (op)
      (case op
        (:read
         (when (and table
                    (table-open-p table))
           (table-close table))
         (when file
           (close-hdf-file file))
         (setf file nil)
         (setf table
               (wrap-for-reuse
                (open-hdf-table-chain (list path) group)
                `(hdf-opener ',path ',fields-specs
                             :buffer-size ',buffer-size
                             :group ',group)))
         table)
        (:write
         (when (and table
                    (table-open-p table))
           (table-close table))
         (when file
           (close-hdf-file file))
         (setf file
               (open-hdf-file path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create))
         (setf table
               (apply #'create-hdf-table
                      file group fields-specs
                      (when buffer-size
                        (list :buffer-size buffer-size))))
         table)))))

(defun hdf-chain-opener (paths
                         &key
                           (group "/table"))
  "Returns a closure which returns an hdf-table-chain for reading and
nil for writing."
  (let ((table nil))
    (lambda (op)
      (case op
        (:read
         (when (and table
                    (table-open-p table))
           (table-close table))
         (setf table
               (wrap-for-reuse
                (open-hdf-table-chain paths group)
                `(hdf-chain-opener ',paths :group ',group)))
         table)
        (:write nil)))))

;; PAW ntuple:
(defun ntuple-opener (path fields-specs)
  "Returns a closure which, when given a single keyword argument,
  returns an ntuple-table ready for reading/writing, automatically
  managing file access and calling table-close when necessary."
  (let ((table nil))
    (lambda (op)
      (case op
        (:read
         (when (and table
                    (table-open-p table))
           (table-close table))
         (setf table
               (wrap-for-reuse
                (open-ntuple-table path fields-specs)
                `(ntuple-opener ',path ',fields-specs)))
         table)
        (:write
         (when (and table
                    (table-open-p table))
           (table-close table))
         (setf table
               (create-ntuple-table path
                                    fields-specs))
         table)))))

;; CSV:
(defun csv-opener (path
                   &key
                     field-names
                     read-from-string
                     (delimeter #\,))
  "Returns a closure which, when given a single keyword argument,
  returns an ntuple-table ready for reading/writing, automatically
  managing file access and calling table-close when necessary."
  (let ((table nil))
    (lambda (op)
      (case op
        (:read
         (when (and table
                    (table-open-p table))
           (table-close table))
         (setf table
               (wrap-for-reuse
                (open-csv-table path
                                :read-from-string read-from-string
                                :delimeter delimeter)
                `(csv-opener ',path
                             :field-names ',field-names
                             :read-from-string ',read-from-string
                             :delimeter ',delimeter)))
         table)
        (:write
         (when (and table
                    (table-open-p table))
           (table-close table))
         (setf table
               (create-csv-table path field-names delimeter))
         table)))))

;; plist-table:

(defun plist-opener (plists)
  (let* ((plists plists)
         (opener-form `(plist-opener ',plists))
         (table (wrap-for-reuse
                 (open-plist-table plists)
                 opener-form))
         (field-names
          (mapcar #'first
                  (group (first plists) 2))))
    (lambda (op)
      (case op
        (:read
         (when (and table
                    (table-open-p table))
           (table-close table))
         (setf plists
               (coerce (plist-table-plists
                        (if (typep table 'reusable-table)
                            (internal-table table)
                            table))
                       'list))
         (setf table
               (wrap-for-reuse
                (open-plist-table plists)
                opener-form))
         table)
        (:write
         (when (and table
                    (table-open-p table))
           (table-close table))
         (setf table
               (create-plist-table field-names))
         table)))))
