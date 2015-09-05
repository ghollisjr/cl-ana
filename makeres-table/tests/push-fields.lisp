(require 'cl-ana)

(in-package :cl-ana)

(defproject push-fields
    "/home/ghollisjr/test/makeres-table/push-fields"
  (list #'macrotrans #'tabletrans #'progresstrans)
  (fixed-cache 5))

(setf *print-pretty* 10)

(with-open-hdf-file (file (work-path "src.h5")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
  (let ((result
         (create-hdf-table file "/table"
                           (list (cons "X" :double)
                                 (cons "Y" :double)))))
    (loop
       for i below 100
       for j downfrom 99
       do (table-push-fields result
            (x (->double-float i))
            (y (->double-float j))))
    (table-close result)
    t))

(defres (src)
  (srctab (hdf-chain-opener (list (work-path "src.h5"))
                            :group "/table")))

(defres (src scrambled)
  (tab (res (src))
      ()
      (hdf-opener (work-path "src-scrambled.h5")
                  (list (cons "X" :double)
                        (cons "Y" :double)))
    (if (< (field x) 50)
        (push-fields
         (x (field x))
         (y (field y)))
        (push-fields
         (x (field y))
         (y (field x))))))
