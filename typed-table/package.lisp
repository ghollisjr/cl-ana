;;;; package.lisp

(defpackage #:typed-table
  (:use #:cl
	#:list-utils
	#:string-utils
        #:symbol-utils
	#:cffi
	#:table
	#:typespec
	#:alexandria)
  (:export :do-typed-table
	   :do-typed-table-marked
           :typed-table-set-field
           ;; could implement typed-table-get-field but may not be
           ;; necessary
	   :typed-table
	   :typed-table-column-specs
           :typed-table-row-cstruct
           :typed-table-row-pointer
           :typed-table-lisp->c-converter-map
           :typed-table-c->lisp-converter-map
	   :typespec->column-names
	   :typespec->column-specs
	   :typed-table->typespec))
