;;;; package.lisp

(defpackage #:hdf-table
  (:use #:cl
	#:list-utils
	#:cffi
	#:hdf-file
	#:hdf-cffi
	#:table
	#:rread-table
	#:typed-table
	#:typespec
	#:hdf-typespec
	#:string-utils
	#:functional-utils
	#:alexandria
	#:binary-tree)
  (:export :hdf-table
	   :open-hdf-table
	   :make-hdf-table
	   :hdf-table-chain
	   :open-hdf-table-chain))
