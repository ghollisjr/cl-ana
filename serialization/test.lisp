(require 'cl-ana)

(in-package :cl-ana)

(with-open-hdf-file (file "/home/ghollisjr/hist.h5"
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
  (write-histogram
   (make-contiguous-hist (list (list :name "x"
                                     :low -5d0
                                     :high 5d0
                                     :nbins 10)))
   file
   "/histogram"))
