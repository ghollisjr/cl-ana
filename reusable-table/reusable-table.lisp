;;;; reusable-table.lisp

;;;; reusable-table provides a wrapper macro which, when given the
;;;; form you would have evaluated to open your table for reading,
;;;; creates a closure which will be called whenever the table needs
;;;; reloading.  The table object returned only defines minimal
;;;; interface, but this is a minor inconvenience as it is still
;;;; possible to access the raw table directly.
;;;;
;;;; It can safely be used with do-table since table-load-next-row
;;;; still returns nil when appropriate.

(in-package :reusable-table)

(defclass reusable-table ()
  ((creation-fn
    :initarg :creation-fn
    :initform nil
    :accessor reusable-table-creation-fn
    :documentation "Function which creates the table being wrapped.")
   (raw-table
    :initarg :raw-table
    :initform nil
    :accessor reusable-table-raw-table
    :documentation "The table which is being wrapped.")
   (needs-reloading
    :initform nil
    :accessor reusable-table-needs-reloading
    :documentation "Boolean which tells the wrapper when it needs to
    close and re-open the table.")))

(defmacro wrap-for-reuse (table-creation-form)
  "Creates a reusable table which places the table-creation-form into
a closure which will be evaluated each time the table gets re-read
from the beginning."
  (let ((lambda-form
         `(function (lambda () ,table-creation-form))))
    `(make-instance 'reusable-table
                    :creation-fn ,lambda-form
                    :raw-table (funcall ,lambda-form))))

(defmethod table-load-next-row ((table reusable-table))
  (with-slots (raw-table creation-fn needs-reloading)
      table
    (when needs-reloading
      (table-close raw-table)
      (setf raw-table (funcall creation-fn))
      (setf needs-reloading nil))
    (let ((status (table-load-next-row raw-table)))
      (when (not status)
        (setf needs-reloading t))
      status)))

(defmethod table-get-field ((table reusable-table) column-symbol)
  (with-slots (raw-table)
      table
    (table-get-field raw-table column-symbol)))

(defmethod table-column-names ((table reusable-table))
  (with-slots (raw-table)
      table
    (table-column-names raw-table)))

;;; Cleanup:

(defmethod table-close ((table reusable-table))
  (with-slots (raw-table needs-reloading)
      table
    (when (not needs-reloading)
      (setf needs-reloading t)
      (table-close raw-table))))
