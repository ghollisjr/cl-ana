;;;; string-utils.lisp

(in-package :string-utils)

(defun lispify (string)
  "Replace all spaces and underscores with -, and convert to
  uppercase."
  (string-upcase (map 'string
		      (lambda (c)
                        (case c
                          (#\Space     #\-)
                          (#\_         #\-)
                          (otherwise   c)))
		      string)))

(defun string-append (&rest strings)
  (apply #'concatenate 'string strings))

;;;; From let-over-lambda:

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))
