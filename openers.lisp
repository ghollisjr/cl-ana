(in-package :makeres-tabletrans)

;;;; This file defines openers for use with tab for various tables
;;;; provided by cl-ana.

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
                (open-hdf-table-chain (list path) group)))
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
         (close-hdf-file file)
         (setf file nil)
         table)))))

;; PAW ntuple:

;; CSV:

;; plist-table:

(defun plist-opener (&optional plists)
  (let ((table nil)
        (plists plists))
    (lambda (op)
      (case op
        (:read
         (when (and table
                    (table-open-p table))
           (table-close table))
         (setf table
               (wrap-for-reuse
                (open-hdf-table-chain (list path) group)))
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
         (close-hdf-file file)
         (setf file nil)
         table)))))
