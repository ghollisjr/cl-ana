(defpackage #:cl-ana
  (:use :cl))
;; :binary-tree
;; :csv-table
;; :math-functions
;; :tensor
;; :error-propogation
;; :file-utils
;; :fitting
;; :functional-utils
;; :gnuplot-i-cffi
;; :gsl-cffi
;; :hdf-cffi
;; :hdf-table
;; :hdf-typespec
;; :hdf-utils
;; :histogram
;; :linear-algebra
;; :list-utils
;; :lorentz
;; :macro-utils
;; :map
;; :memoization
;; :ntuple-table
;; :plotting
;; :reusable-table
;; :statistics
;; :string-utils
;; :symbol-utils
;; :symbol-utils
;; :table
;; :typed-table
;; :typespec))

(in-package :cl-ana)

(defvar *cl-ana-package-names*
  (list :package-utils
        :generic-math
        :binary-tree
        :csv-table
        :math-functions
        :tensor
        :error-propogation
        :file-utils
        :fitting
        :functional-utils
        :gnuplot-i-cffi
        :gsl-cffi
        :hdf-cffi
        :hdf-table
        :hdf-typespec
        :hdf-utils
        :histogram
        :linear-algebra
        :list-utils
        :lorentz
        :macro-utils
        :map
        :memoization
        :ntuple-table
        :plotting
        :reusable-table
        :statistics
        :string-utils
        :symbol-utils
        :symbol-utils
        :table
        :table-viewing
        :typed-table
        :typespec))

(loop
   for p in *cl-ana-package-names*
   do (package-utils:add-package-to-group p :cl-ana))

(package-utils:use-package-group :cl-ana)

;;(gmath:use-gmath :cl-ana)
