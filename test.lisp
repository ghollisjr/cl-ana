(in-package :makeres)

(in-project test)

(defres filtered ((source (list 1 2 3 4 5 6 7)))
  (print 'filtered)
  (remove-if (lambda (x)
               (< x 5))
             source))

(defres squared ()
  (print 'squared)
  (mapcar (lambda (x)
            (* x x))
          (res filtered)))

(defres sum-scaled (scale)
  (print 'sum-scaled)
  (* scale
     (+ (res filtered)
        (res squared))))
