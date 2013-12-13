(require 'reusable-table)

(require 'hdf-table)

(in-package :hdf-table)

(use-package 'reusable-table)

(defvar *table*
  (wrap-for-reuse
   (open-hdf-table-chain (list "/home/ghollisjr/hdfwork/outfile.h5")
                         "/output-dataset")))

(do-table (i *table*)
    ("x" "y")
  (when (zerop i)
    (print "First pass: ")
    (print x)
    (print y)))

(do-table (i *table*)
    ("x" "y")
  (when (zerop i)
    (print "Second pass: ")
    (print x)
    (print y)))
