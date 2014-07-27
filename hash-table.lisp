(in-package logres)

;; For now, hash-tables are converted into alists, but in the future
;; it would be nice if all container objects would use their storage
;; file as an index, allowing e.g. lists of histograms to be stored
;; (histograms are stored via HDF5, not text files).

(defmethod save-object ((ht hash-table) path)
  (with-open-file (file path
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (format file "~s~%" (map->alist ht))
    (format file "~s~%" (hash-table-test ht))))

(defmethod load-object ((type (eql 'hash-table)) path)
  (with-open-file (file path
                        :direction :input
                        :if-does-not-exist :error)
    (let ((alist (read file))
          (test (read file)))
      (map->hash-table alist test))))
