(require 'makeres-graphviz)

(in-package :makeres-graphviz)

(in-project makeres-graphviz)

(defres src
  (list 1 2 3 4 5))

(defres squares
  (mapcar (lambda (x)
            (expt x 2))
          (res src)))

(defres mean
  (mean (res src)))

(defres mean-squares
  (mean (res squares)))

(defres variance
  (- (res mean-squares)
     (expt (res mean)
           2)))

(defres standard-deviation
  (sqrt (res variance)))
