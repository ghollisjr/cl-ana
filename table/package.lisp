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
	   :table-commit-row
	   :table-close
	   :do-table
	   ;; table-chain:
	   :open-table-chain
	   :reset-table-chain
	   ;; plist-table:
	   :open-plist-table))
