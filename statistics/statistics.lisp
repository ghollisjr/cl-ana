(in-package :statistics)

(defun mean (data)
  (/ (sum data)
     (length data)))

(defun moving-average (data subset-length)
  "Returns a list of the moving averages on the data with subsets of
length subset-length.  subset-length is not checked to be in bounds,
so be careful."
  (do* (())))
