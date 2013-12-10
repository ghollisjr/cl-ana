;;;; package.lisp

(defpackage #:table
  (:use #:cl
	#:list-utils
	#:macro-utils
	#:string-utils
	#:functional-utils
	#:alexandria)
  (:export :table
	   :table-column-names
	   :table-access-mode
	   :table-column-symbols
	   :table-load-next-row
	   :table-get-field
	   :table-set-field
           :table-push-fields
	   :table-commit-row
	   :table-close
	   :do-table
	   :do-table-marked
	   ;; table-chain:
	   :open-table-chain
	   :reset-table-chain
	   ;; plist-table:
	   :open-plist-table))
