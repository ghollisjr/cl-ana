;;;; string-utils.lisp

(in-package :string-utils)

(defun lispify (string)
  "Replace all spaces and underscores with -, and convert to
  uppercase."
  (string-upcase (map 'string
		      #'(lambda (c)
			  (case c
			    (#\Space     #\-)
			    (#\_         #\-)
			    (otherwise   c)))
		      string)))
