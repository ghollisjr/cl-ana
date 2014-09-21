(in-package logres)

(defmethod save-target (lid (cell cons) path)
  (let ((savedir
         (make-pathname
          :directory (pathname-directory path))))

    (let ((car-id (next-log-id))
          (car-type (target-type (car cell)))
          (cdr-id (next-log-id))
          (cdr-type (target-type (cdr cell))))
      (symbol-macrolet ((sublids
                         (gethash lid
                                  (gethash *project-id*
                                           *proj->lid->sublids*))))
        (push car-id sublids)
        (push cdr-id sublids)
        (with-open-file (file path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
          (format file "~a~%"
                  (cons (list car-id car-type)
                        (list cdr-id cdr-type))))
        (save-target car-id
                     (car cell)
                     (merge-pathnames (mkstr car-id)
                                      savedir))
        (save-target cdr-id
                     (cdr cell)
                     (merge-pathnames (mkstr cdr-id)
                                      savedir))))))

(defmethod load-target ((type (eql 'cons)) path)
  (let ((loaddir
         (make-pathname
          :directory (pathname-directory path))))
    (let ((index-cons nil))
      (with-open-file (file path
                            :direction :input
                            :if-does-not-exist :error)
        (setf index-cons (read file)))
      (destructuring-bind ((car-id car-type) . (cdr-id cdr-type))
          index-cons
        (cons (load-target car-type
                           (merge-pathnames
                            (mkstr car-id)
                            loaddir))
              (load-target cdr-type
                           (merge-pathnames
                            (mkstr cdr-id)
                            loaddir)))))))
