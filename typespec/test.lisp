(require 'typespec)

(in-package :typespec)

(defparameter *typespec* (list :compound
                               (cons "x" :int)
                               (cons "y" :double)))

(defparameter *class* (compound-typespec->class *typespec*))

(defparameter *x* (make-instance *class* :x 3 :y 4))
