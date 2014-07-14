(in-package :trl)

;;;; trl (tagged result logger) is a result database tool for
;;;; automating the storage and retrieval of computed results.
;;;; Typical use is in data analysis where there is a mountain of
;;;; results needing to be organized.
;;;;
;;;; trl is useful in conjunction with cl-ana and makeres.
;;;;
;;;; Example: Tables & reductions: makeres can be told how to optimize
;;;; looping over tables, generating result tables along with any
;;;; other reductions according to the dependency graph.  Via macros,
;;;; operators can be defined which define trl entries for reductions
;;;; with ids created automatically via whatever convention you wish
;;;; (I like to use lists which denote the chain of tables/filters/etc
;;;; along with a reduction id).
