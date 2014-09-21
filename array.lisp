(in-package logres)

(defmethod save-target (lid (vector array) path)
  (let ((savedir
         (make-pathname
          :directory (pathname-directory path))))
    (with-open-file (file path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (format file
              "~a~%"
              (loop
                 for elt across vector
                 collect
                   (symbol-macrolet ((sublids
                                      (gethash lid
                                               (gethash *project-id*
                                                        *proj->lid->sublids*))))
                     (let ((elt-id (next-log-id))
                           (elt-type (target-type elt)))
                       (push elt-id sublids)
                       (save-target elt-id
                                    elt
                                    (merge-pathnames (mkstr elt-id)
                                                     savedir))
                       (list elt-id elt-type))))))))

(defmethod load-target ((type (eql 'array)) path)
  (let ((loaddir
         (make-pathname
          :directory (pathname-directory path))))
    (with-open-file (file path
                          :direction :input
                          :if-does-not-exist :error)
      (let ((index-list (read file)))
        (map 'vector
             (lambda (elt)
               (destructuring-bind (elt-id elt-type)
                   elt
                 (load-target elt-type
                              (merge-pathnames
                               (mkstr elt-id)
                               loaddir))))
             index-list)))))
