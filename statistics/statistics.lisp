(in-package :statistics)

(defun mean (data)
  "Returns mean (and count) of data"
  (loop
     for x in data
     summing x into sum
     counting t into num
     finally (return (values (/ sum num) num))))

(defun variance (data)
  (multiple-value-bind (mean count)
      (mean data)
    (/ (sum (mapcar (lambda (x) (expt (- x mean) 2))
                    data))
       (- count 1))))

(defun standard-deviation (data)
  (sqrt (variance data)))

(defun skewness (data))

(defun kirtosis (data))

(defun quantiles (data)
  "Returns an alist mapping each datum to its quantile."
  (let ((denom (+ 1 (length data)))
        (compressed
         (compress data :test #'equal :singleton-pairs t))
        (acc 0d0))
    (loop
       for (x . c) in compressed
       do (incf acc c)
       collecting (cons x (/ acc denom)) into result
       finally (return result))))

(defun percentiles (data)
  "Returns an alist mapping each datum to its percentile."
  (let ((denom (length data))
        (compressed
         (compress (sort (copy-list data) #'<)
                   :test #'equal :singleton-pairs t))
        (acc 0d0))
    (loop
       for (x . c) in compressed
       do (incf acc c)
       collecting (cons x (/ acc denom)) into result
       finally (return result))))

(defun mean-accumulator (&rest sample)
  "Returns two values:

1. A function which returns the (updated) moving/running average each
time you call it on a value.  The samples you give provide the
initilization data and the size of the data subset to maintain while
computing the moving average.

2. The initial value for the moving average; this is so that
subsequent calls on the moving average function will return the
immediate updated mean."
  (let* ((lst (copy-list sample))
         (length (length lst))
         (end (last lst))
         (initial-mean (/ (sum lst)
                          length)))
    (values (lambda (x)
              (setf (cdr end)
                    (list x))
              (setf end
                    (cdr end))
              (setf lst
                    (rest lst))
              (/ (sum lst) length))
            initial-mean)))

(defun moving-average (data subset-length)
  "Returns a list of the moving averages on the data (must be a list)
with subsets of length subset-length.  subset-length is not checked to
be in bounds, so be careful."
  (multiple-value-bind (acc init-mean)
      (apply #'mean-accumulator (subseq data 0 subset-length))
    (cons init-mean
          (mapcar acc
                  (subseq data subset-length)))))
