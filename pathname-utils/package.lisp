(defpackage #:pathname-utils
  (:use :cl)
  (:export :pathname-absolute-p
           :pathname-relative-p
           :->absolute-pathname))
