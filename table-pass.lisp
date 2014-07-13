(in-package :makeres-tabletrans)

;;;; NOTES
;;;;
;;;; * The lfields argument is necessary in the way the merge
;;;;   algorithm is currently written.  There is however latent
;;;;   functionality which allows for individual table-pass form
;;;;   specific lfields in the algorithm which is unavailable to the
;;;;   user if they use the dotab operator; it is not a loss due to
;;;;   being able to use the let operator in the loop body, but this
;;;;   note is for future reference if concerns about lfields arise.

;; general purpose table iteration, more functional than do-table,
;; used as backbone for all makeres-tabletrans transformations
(defmacro table-pass (table inits result lfields &body body)
  "Loops over table with external bindings inits and result form
result, executing body once per row.

macro field yields the field value of current row.

macro row-number yields the row number of current row.

Limitations: Make sure no forms (field X) occur which are not meant to
reference the field value.  I've tried various options to make this
work via macros but nothing short of code walking looks viable, and
this is my naive code walking strategy's fault.

When used with makeres, each table-pass is guaranteed to have
independent lfields and inits, no matter what symbol names you choose.
If you need a common lfield, use deflfields.  If you need a common
init binding, at the moment the only solution is to combine the
targets manually (usually more conceptually clear incidentally)."
  ;; local macro fields will accept either symbol or string, and will
  ;; convert a symbol into a lower-case string for use in fields.

  ;; Having difficulties expanding the body to get the fields which
  ;; are present, trying to use &environment with expand macro but not
  ;; much luck so far.
  (let ((lfield->gsym
         (let ((result (make-hash-table :test 'equal)))
           (loop for i in lfields
              do (setf (gethash (first i) result)
                       (gensym)))
           result)))
    (alexandria:with-gensyms (ri)
      `(macrolet ((field (field-sym)
                    (or (gethash field-sym ,lfield->gsym)
                        (intern (lispify field-sym)))))
         (let* ,inits
           (do-table (,ri ,table)
               ,(list->set (mapcar (alexandria:compose #'intern
                                                       #'lispify)
                                   (remove-if
                                    (lambda (x)
                                      (gethash x lfield->gsym))
                                    (append (makeres::find-dependencies lfields 'field)
                                            (makeres::find-dependencies body 'field))))
                           #'string=)
             (olet ,(loop for i in lfields
                       collect `(,(gethash (first i) lfield->gsym)
                                  ,(second i)))
               ,@(remove-if-not (lambda (x)
                                  (and (listp x)
                                       (eq (first x)
                                           'declare)))
                                body)
               (flet ((row-number ()
                        ,ri))
                 ,@(remove-if (lambda (x)
                                (and (listp x)
                                     (eq (first x)
                                         'declare)))
                              body))))
           ,result)))))
