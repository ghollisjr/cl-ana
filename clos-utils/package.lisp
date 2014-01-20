(defpackage #:clos-utils
  (:use :cl
        :list-utils
        :symbol-utils)
  (:export :slot-names
           :slot-keyword-names
           :slot-values
           :clist-type
           :clist-field-symbols
           :clist-field-values
           :object->clist
           :clist->object
           :type-constructor))
