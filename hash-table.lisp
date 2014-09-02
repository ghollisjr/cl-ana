(in-package logres)

;; For now, hash-tables are converted into alists, but in the future
;; it would be nice if all container objects would use their storage
;; file as an index, allowing e.g. lists of histograms to be stored
;; (histograms are stored via HDF5, not text files).

(defmethod save-object ((ht hash-table) path)
  (let ((savedir
         (make-pathname
          :directory (pathname-directory path))))
    (with-open-file (file path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (let ((index-alist nil))
        (loop
           for k being the hash-keys in ht
           for v being the hash-values in ht
           do (let ((kid (next-log-id))
                    (vid (next-log-id)))
                (push (cons (list kid (type-of k))
                            (list vid (type-of v)))
                      index-alist)
                (save-object k
                             (merge-pathnames (mkstr kid)
                                              savedir))
                (save-object v
                             (merge-pathnames (mkstr vid)
                                              savedir))))
        (format file "~s~%" (nreverse index-alist))
        (format file "~s~%" (hash-table-test ht))))))

(defmethod load-object ((type (eql 'hash-table)) path)
  (let ((loaddir
         (make-pathname
          :directory (pathname-directory path))))
    (with-open-file (file path
                          :direction :input
                          :if-does-not-exist :error)
      (let ((index-alist (read file))
            (test (read file)))
        (map->hash-table
         (mapcar (lambda (cons)
                   (destructuring-bind ((kid ktype) . (vid vtype))
                       cons
                     (cons (load-object ktype
                                        (merge-pathnames
                                         (mkstr kid)
                                         loaddir))
                           (load-object vtype
                                        (merge-pathnames
                                         (mkstr vid)
                                         loaddir)))))
                 index-alist)
         test)))))
