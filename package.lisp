(defpackage #:makeres-tabletrans
  (:use :cl
        :makeres)
  (:export
   :tabletrans
   :*tabletrans-symmap*))

(package-utils:use-package-group :cl-ana :makeres-tabletrans)
