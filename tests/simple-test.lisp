(require 'cl-ana)

(require 'makeres)

(require 'makeres-table)

(defpackage #:simple-test
  (:use :cl
        :makeres
        :makeres-table))

(package-utils:use-package-group :cl-ana :simple-test)

(in-package simple-test)

(in-project simple-test)

(settrans (list #'tabletrans) :op :set)

(defres source
  (wrap-for-reuse
   (open-plist-table
    (loop
       for i below 10000000
       collecting (list :x i)))))

(deflfields source
    ((y (* 2 (field x)))))

(defres sum
  (dotab (res source)
      ((sum (progn
              (format t "starting sum~%")
              0)))
      sum
    (incf sum (field x))))

(defres count
  (dotab (res source)
      ((count (progn
                (format t "starting count~%")
                0)))
      count
    (incf count)))

(defres mean
  (format t "starting mean~%")
  (/ (res sum)
     (res count)))

(defres filtered
  (ltab (res source) ()
    (when (< (field x) (res mean))
      (push-fields
       (x (field x))))))

;; works up to this point, breaks after the following:

(defres (filtered sum)
  (dotab (res filtered)
      ((sum (progn
              (format t "starting filtered sum~%")
              0)))
      sum
    (incf sum (field x))))

(defres (filtered count)
  (dotab (res filtered)
      ((count (progn
                (format t "starting filtered count~%")
                0)))
      count
    (incf count)))

(defres (filtered mean)
  (format t "starting filtered mean~%")
  (/ (res (filtered sum))
     (res (filtered count))))
