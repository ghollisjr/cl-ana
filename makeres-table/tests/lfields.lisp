(require 'cl-ana)

(in-package :cl-ana)

(defproject lfields
    "/home/ghollisjr/test/makeres-table/lfields/"
  (list #'macrotrans #'tabletrans)
  (fixed-cache 5))

(setf *print-progress* 10)

(defres (bootstrap src)
  (with-open-hdf-file (file (work-path "src.h5")
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
    (let ((tab
              (create-hdf-table file "/table"
                                (list (cons "X" :double)
                                      (cons "Y"
                                            (list :array :double 2))))))
      (loop
         for i from 1 to 100
         do (let* ((x (->double-float i))
                   (y (vector (log x)
                              (exp x))))
              (table-push-fields
                  tab
                (x x)
                (y y))))
      (table-close tab))))

(defres (src)
  (srctab
   (hdf-chain-opener (list (work-path "src.h5")))
   (res (bootstrap src))))

(deflfields (src)
    ((ysum (sum (field y)))
     (sum (+ (field x)
             (field ysum)))
     (z (sqrt (field sum)))))

(defres (proc)
  (tab (res (src))
      ()
      (hdf-opener (work-path "proc.h5")
                  (list (cons "X" :double)
                        (cons "Y"
                              (list :array :double 2))
                        (cons "Z" :double)))
    (push-fields
     (x (field x))
     (y (field y))
     (z (field z)))))

(deflfields (proc)
    ((ysum (sum (field y)))
     (sum (+ (field x)
             (field ysum)))))

(defres (filtered)
  (ltab (res (proc)) ()
    (when (< (field x) 50)
      (push-fields
       (x (field x))
       (y (field y))
       (sum (field sum))
       (z (field z))))))

(defres (filtered total-sum)
  (dotab (res (filtered))
      ((sum 0))
      sum
    (incf sum
          (field sum))))

(load-project)
(makeres)
