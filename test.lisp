(in-package :makeres-tabletrans)

;; NOTE: tabletrans is an example of a graph transformation which
;; needs to have makeres run even when the only thing updated is the
;; input parameters.

;; Demo of tabletrans

(in-project transform)

(settrans (tabletrans))

(defpars
    (nrows 100))

;; source table
(defres table
  (wrap-for-reuse
   (open-plist-table
    (mapcar (lambda (x)
              (list :x x))
            (loop
               for i below (par nrows)
               collecting i)))))

;; average:
(defres mean
  (table-pass (res table)
      ((sum 0)
       (count 0))
      (progn
        (print 'return-mean)
        (the float (float (/ sum count))))
    (print 'loop-mean)
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
      (the float
           (float (/ sum-squares
                     (1- count))))
    (incf sum-squares
          (expt (- (field x)
                   (res mean))
                2))
    (incf count)))

(defres sigma
  (sqrt (res variance)))
