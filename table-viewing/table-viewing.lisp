(in-package :table-viewing)

(defun table-view (table fields bin-specs &key
                                            (processor #'list)
                                            (filter (constantly t)))
  "table-view plots & histograms processed data from a table; note
that a reusable-table should be used generally since this does exhaust
the table.  It plots & returns (via multiple values) the
histogram (contiguous) & page for the plot.

fields is a list of fields to select from the table.

bin-specs is a list of bin-spec lists: Each bin-spec is a plist with
slots :name, :nbins, :low and :high.  These are passed directly to
make-contiguous-hist.

processor generates the list of values suitable for insertion into the
histogram.  It should take as many arguments as the fields selected;
the fields will be applied to fn in the order specified.  By default
it simply returns the list of selected fields.

filter is an optional function which takes the selected fields as
arguments and returns t whenever the event should be included in the
viewing.  By default it always returns t."
  (let* ((field-symbols (mapcar (compose #'intern #'lispify)
                                fields))
         (hist (make-contiguous-hist bin-specs)))
    (table-reduce table fields
                  (lambda (&rest xs)
                    (when (apply filter (rest xs))
                      (hist-insert hist
                                   (mklist (apply processor (rest xs)))))))
    (values hist
            (quick-draw hist))))

(defmacro table-easy-view (table fields bin-specs &key
                                                    processor
                                                    filter)
  "Similar to table-view, but allows processor and filter to be
expressions which will be used as a body in the appropriate lambda
functions when specified.

fields is not evaluated.

bin-specs is evaluated."
  (let ((field-symbols (mapcar (compose #'intern #'lispify #'string)
                               fields)))
    `(apply #'table-view ,table ',fields ,bin-specs
            (list
             ,@(when-keywords (:processor (when processor
                                            `(lambda (,@field-symbols)
                                               ,processor)))
                              (:filter (when filter
                                         `(lambda (,@field-symbols)
                                            ,filter))))))))
