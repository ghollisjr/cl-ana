(defpackage #:logres
  (:use :cl
        :makeres
        :external-program)
  (:export :load-object
           :save-object
           :project-path
           :set-project-path
           :save-project
           :load-project
           :logres-ignore
           :logres-ignorefn
           :logres-track
           :logres-trackfn))

(package-utils:use-package-group :cl-ana :logres)
