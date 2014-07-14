(in-package :makeres-tabletrans)

(in-project pass-collapse)

(settrans (pass-collapse pass-merge))

(defres source
  (wrap-for-reuse
   (open-plist-table '((:x 1)
                       (:x 2)
                       (:x 3)))))

(defres filtered
  (ltab (res source)
      ()
    (when (< (field x) 4)
      (push-fields
       ;;(x (field x))
       (y (* 2 (field x)))))))

(defres filtered2
  (ltab (res source)
      ()
    (when (> (field x) 5)
      (push-fields
       (y (field x))))))

(defres canon
  (tab (res filtered)
      (hdf-opener "/home/ghollisjr/canon.h5"
                  '(("x" . :int)
                    ("y" . :float)
                    ("z" . :float)))
      ()
    (push-fields (x (field y))
                 (y (sqrt (field y)))
                 (z (float
                     (expt (field y)
                           2))))))
