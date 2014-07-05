(in-package :makeres-tabletrans)

;; Demo of tabletrans

(in-project transform)

(settrans (tabletrans))

;; source table
(defres table
  (wrap-for-reuse
   (open-plist-table
    (mapcar (lambda (x)
              (list :x x))
            (loop
               for i below 100
               collecting i)))))

;; average:
(defres mean
  (table-pass (res table)
      ((sum 0)
       (count 0))
      (/ sum count)
    (incf sum (field x))
    (incf count)))

(defres max
  (table-pass (res table)
      ((max nil))
      max
    (if (not max)
        (setf max (field x))
        (when (> (field x) max)
          (setf max (field x))))))


(defres variance
  (table-pass (res table)
      ((sum-squares 0)
       (count 0)) ; safe since variance happens in second pass
      (/ sum-squares
         (1- count))
    (incf sum-squares
          (expt (- (field x)
                   (res mean))
                2))
    (incf count)))

(defres sigma
  (sqrt (res variance)))
