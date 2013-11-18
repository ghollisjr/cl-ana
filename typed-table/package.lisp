;;;; package.lisp

(defpackage #:typed-table
  (:use #:cl
	#:string-utils
	#:list-utils
	#:cffi
	#:table
	#:typespec
	#:alexandria)
  (:export :do-typed-table
	   :do-typed-table-marked
	   :typed-table
	   :typed-table-column-specs
	   :typespec->column-names
	   :typespec->column-specs
	   :typed-table->typespec))
