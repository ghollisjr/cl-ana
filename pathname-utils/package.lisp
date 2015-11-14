(defpackage #:cl-ana.pathname-utils
  (:use :cl)
  (:export :pathname-absolute-p
           :pathname-relative-p
           :->absolute-pathname
           :directory-pathname-p
           :basename
           :mkdirpath))
