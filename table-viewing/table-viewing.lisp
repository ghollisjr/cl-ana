(in-package :table-viewing)

(defun table-view (table field-specs &key filter)
  "table-view plots & returns (via multiple values) the
histogram (contiguous) & page for the plot.  It is primarily useful
for quick visualization of data, as more complex code is typically
necessary for final products.

Each field-spec is either the name of the field to plot or a list with
the name of the field as the first element and the rest of the list as
a plist with available slots :nbins, :low and :high.  Each of these
slots are optional; the :low and :high will be determined from the
data if unspecified.")

