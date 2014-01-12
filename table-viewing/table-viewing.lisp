(in-package :table-viewing)

(defun table-view (table field-specs &key (filter (constantly t)))
  "table-view plots & histograms data from a table; note that a
reusable-table should be used generally since this does exhaust the
table.  It plots & returns (via multiple values) the
histogram (contiguous) & page for the plot.  It is primarily useful
for quick visualization of data, as more complex code is typically
necessary for final products.

Each field-spec is a plist with slots :name, :nbins, :low and :high.
These are passed directly to make-contiguous-hist.

filter is an optional function which takes the selected fields as
arguments and returns t whenever the event should be included in the
viewing."
  (let* ((field-names (mapcar (compose #'string
                                       (lambda (x)
                                         (getf x :name)))
                              field-specs))
         (field-symbols (mapcar (compose #'intern #'lispify)
                                field-names))
         (hist (make-contiguous-hist field-specs)))
    (table-reduce table field-names
                  (lambda (&rest xs)
                    (when (apply filter (rest xs))
                      (hist-insert hist
                                   (rest xs)))))
    (values hist
            (quick-draw hist))))
