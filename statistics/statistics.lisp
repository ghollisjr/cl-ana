(in-package :statistics)

(defun mean (data)
  (/ (sum data)
     (length data)))

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
  "Returns a list of the moving averages on the data with subsets of
length subset-length.  subset-length is not checked to be in bounds,
so be careful."
  (multiple-value-bind (acc init-mean)
      (apply #'mean-accumulator (subseq data 0 subset-length))
    (cons init-mean
          (mapcar acc
                  (subseq data subset-length)))))
