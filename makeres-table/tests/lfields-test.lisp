(require 'makeres-table)

(in-package :makeres-table)

(in-project lfields-test)

(settrans (list #'macrotrans #'tabletrans) :op :set)

(defres src
  (wrap-for-reuse
   (open-plist-table (list (list :x 1)
                           (list :x 2)
                           (list :x 3)))))

(deflfields src
    ((y (+ (field x)
           1))))

(defres tab
  (tab (res src)
      ()
      (hdf-opener "lfields-test.h5"
                  (list (cons "x" :int)
                        (cons "y" :int)))
    (push-fields
     (x (field x))
     (y (field y)))))

(deflfields tab
    ((z (+ (field x)
           (field y)))))

(defres (mean tab z)
  (dotab (res tab)
      ((sum 0)
       (count 0))
      (/ sum count)
    (incf sum (field z))
    (incf count)))
