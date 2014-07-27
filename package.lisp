(defpackage #:logres
  (:use :cl
        :makeres
        :external-program)
  (:export :load-object
           :save-object
           :set-project-path
           :save-project))

(package-utils:use-package-group :cl-ana :logres)
