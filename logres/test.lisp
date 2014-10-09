(require 'logres)

(in-package logres)

(in-project logres-test)

(set-project-path "/home/ghollisjr/logres-test/")

(defres source
  (list (list :x 1)
        (list :x 2)
        (list :x 3)))

(defres vector
  (vector 1 2 3 4 5))

(defres (mean x)
  (/ (sum (mapcar (lambda (x)
                    (getf x :x))
                  (res source)))
     (length (res source))))

(defres hash-table
  (let ((hash-table (make-hash-table :test 'equal)))
    (setf (gethash 'x hash-table)
          'x)
    hash-table))

(defres nested
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash 'hash-table ht)
          (res hash-table))
    ht))
