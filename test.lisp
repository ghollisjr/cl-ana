(require 'logres)

(in-package logres)

(in-project logres-test)

(set-project-path "/home/ghollisjr/logres-test/")

(defres source
  (list (list :x 1)
        (list :x 2)
        (list :x 3)))

(defres (mean x)
  (/ (sum (mapcar (lambda (x)
                    (getf x :x))
                  (res source)))
     (length (res source))))
