(defpackage #:logres
  (:use :cl
        :makeres
        :external-program)
  (:export :load-object
           :save-object
           :set-project-path
           :save-project
           :logres-ignore
           :logres-ignorefn))

(package-utils:use-package-group :cl-ana :logres)
