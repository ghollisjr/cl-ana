(require 'logres)
(require 'makeres)

(defpackage #:function-test
  (:use :cl
        :makeres
        :logres))

(package-utils:use-package-group :cl-ana :function-test)

(in-package function-test)

(in-project function-test)

(set-project-path "function-test/")

(logres-ignore-by #'functionp)

(defres f
  (lambda (x) (print x)))
