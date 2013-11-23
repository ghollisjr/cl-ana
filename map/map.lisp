;;;; map.lisp

(in-package :map)

;;;; A map is any object which represents or can be represented by the
;;;; mapping from one value to another; a two good examples are alists
;;;; and hash tables.
;;;;
;;;; These utilities are for the generic handling of maps so they can
;;;; be appropriately treated by things like fitting data or making
;;;; scatter plots.

(defgeneric map->alist (object)
  (:documentation "Returns the values of a data-source as an alist
  mapping the independent value to the dependent one.
  Multidimensional independent variable data can be represented as a
  list of values.")
  (:method ((obj list))
    obj)
  (:method ((obj hash-table))
    (loop
       for k being the hash-keys of obj
       for v being the hash-values of obj
       collect (cons k v))))
