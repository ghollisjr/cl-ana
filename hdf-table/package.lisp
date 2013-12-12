;;;; package.lisp

(defpackage #:hdf-table
  (:use #:cl
	#:list-utils
	#:cffi
	#:hdf-utils
	#:hdf-cffi
	#:table
	#:typed-table
	#:typespec
	#:hdf-typespec
	#:string-utils
	#:functional-utils
	#:alexandria
	#:binary-tree)
  (:export :hdf-table
           :hdf-table-nrows
	   :open-hdf-table
	   :make-hdf-table
	   :hdf-table-chain
           :hdf-table-chain-nrows
	   :open-hdf-table-chain))
