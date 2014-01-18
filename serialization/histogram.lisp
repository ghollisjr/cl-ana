(in-package :serialization)

(defparameter *histogram-data-path* "data")

(defparameter *histogram-bin-spec-path* "bin-specs")

(defun ->double-float (x)
  (float x 0d0))

;; This is actually broken in principle since I need to also store the
;; names-specs in the file as well; my best guess is to use the
;; hdf-path as the group and then place the histogram contents as
;; members/files under the group.
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
           (data-column-symbols (table-column-symbols data-table))
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
            (list (cons "name" (list :array :char 1 (list max-string-length)))
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
                 for sym in data-column-symbols
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
                (nbins (getf plist :nbins))
                (low (->double-float (getf plist :low)))
                (high (->double-float (getf plist :high))))))
      (table-close bin-spec-table))))

(defun read-histogram (file hdf-path)
  "Reads a histogram from an hdf-table with file and path.

Note that this function assumes that either all the dimensions have
names or none of them do."
  (flet ((subpath (path)
           (concatenate 'string
                        hdf-path
                        "/"
                        path)))
    (let* ((bin-spec-table
            (open-hdf-table file
                            (subpath *histogram-bin-spec-path*)))
           (bin-spec-table-column-names
            (list "name" "low" "high" "nbins"))
           (bin-spec-plists
            (let ((result ()))
              (table-reduce bin-spec-table
                            bin-spec-table-column-names
                            (lambda (state name low high nbins)
                              (push (list :name (char-vector->string name)
                                          :low low
                                          :high high
                                          :nbins nbins)
                                    result)))
              (nreverse result)))
           (histogram (make-contiguous-hist bin-spec-plists))
           (data-table
            (open-hdf-table file
                            (subpath *histogram-data-path*)))
           (data-table-column-names
            (table-column-names data-table))
           (names-specs nil))
      (table-reduce data-table
                    data-table-column-names
                    (lambda (state count &rest xs)
                      (hist-insert histogram xs count)))
      (table-close bin-spec-table)
      (table-close data-table)
      histogram)))
