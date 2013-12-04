(in-package :symbol-utils)

(defun keywordify (symbol)
  (intern
   (string symbol)
   (package-name :keyword)))
