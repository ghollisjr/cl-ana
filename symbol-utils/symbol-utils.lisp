(in-package :symbol-utils)

(defun keywordify (symbol-or-string)
  "Returns the keyword version of a symbol or string."
  (intern
   (string symbol-or-string)
   (package-name :keyword)))
