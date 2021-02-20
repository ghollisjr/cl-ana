
(in-package :hdf5)

(defparameter +NULL+ (cffi:null-pointer))

(define-foreign-library hdf5
  (:darwin (:or (:framework "hdf5") "libhdf5.dylib")) ;; ?
  (:windows "libhdf5.dll" :convention :stdcall)
  (:unix (:or "libhdf5.so"
              "libhdf5_serial.so"
              "libhdf5_serial.so.100"
              "libhdf5_serial.so.100.0.1")))

(use-foreign-library hdf5)

(defun lispify (name &optional flag (package *package*))
  (labels ((helper (lst last rest &aux (c (car lst)))
             (cond ((null lst)
                    rest)
                   ((upper-case-p c)
                    (helper (cdr lst) 'upper
                            (case last
                              ((lower) (list* c #\- rest))
                              (t (cons c rest)))))
                   ((lower-case-p c)
                    (helper (cdr lst) 'lower (cons (char-upcase c) rest)))
                   ((digit-char-p c)
                    (helper (cdr lst) 'digit (cons c rest)))
                   ((char-equal c #\_)
                    (helper (cdr lst) '_ (cons #\- rest)))
                   (t
                    (error "Invalid character: ~A" c)))))
    (let* ((fix (case flag
                  ((constant enumvalue) "+")
                  (variable "*")
                  (t "")))
           (sym (intern
                 (concatenate 'string
                              fix
                              (nreverse (helper (concatenate 'list name) nil nil))
                              fix)
                 package)))
      ;;(export sym package)
      sym)))

