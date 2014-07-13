(in-package :makeres-tabletrans)

(in-project hdf-read-write)

(settrans (pass-merge))

;; pass-collapse

;; If a logical version of this were created (using lres (table-pass
;; ...)), it would be compatible with pass-collapse.

;; Example of how to use tab and hdf-opener:
(defun tab-hdf-opener-example (outpath)
  "Writes a single-row table to an hdf file at outpath"
  (table-close ; tab returns a table open for reading
   (tab (open-plist-table '((:x 1)))
       (hdf-opener outpath '(("x" . :int)))
       ()
     (push-fields
      (x (field x))))))
