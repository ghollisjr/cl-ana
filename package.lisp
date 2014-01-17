(defpackage #:cl-ana
  (:use :cl))

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
        :typespec
        :int-char
        :clos-utils
        :serialization))

(loop
   for p in *cl-ana-package-names*
   do (package-utils:add-package-to-group p :cl-ana))

(package-utils:use-package-group :cl-ana)
