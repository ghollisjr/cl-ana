(require 'cl-ana)

(in-package :cl-ana)

(defparameter *table*
  (wrap-for-reuse
   (open-plist-table
    (loop
       for i below 10000
       collecting (list 'x (random 30)
                        'y (random 30))))))

(defparameter *hist*
  (table-view *table*
              (list
               (list :name "x"
                     :nbins 30
                     :low 0d0
                     :high 30d0))))

(print (hist-bin-values *hist*))
