(defun test (num-data)
  (let ((data
	 (loop
	    for i below num-data
	    collect (cons i (+ i (- (/ (random 30) 30) 5d-1))))))
    (fitting:fit #'(lambda (p x)
		     (let ((A (first p))
			   (B (second p)))
		       (+ (* A x) B)))
		 data
		 (list 0.0 1.0)
		 :prec 1d-6)))
