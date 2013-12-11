;;;; hdf-table-chain.lisp

(in-package :hdf-table)

;; (declaim (optimize (speed 3)
;;                    (safety 0)
;;                    (compilation-speed 0)
;;                    (debug 0)))

;;;; hdf-table-chain: A read-only chain of hdf-tables which can be
;;;; randomly accessed unlike the general table-chain.

(defclass hdf-table-chain (table)
  ((dataset-path
    :initform ""
    :initarg :dataset-path
    :accessor hdf-table-chain-dataset-path
    :documentation "Path to dataset within each file.  The path should
    be uniform if the hdf files truly are a chain.")
   (active-table ; hdf-table in-memory
    :initform ()
    :accessor hdf-table-chain-active-table
    :documentation "The in-memory hdf-table")
   (active-file ; hdf-file currently opened
    :initform nil
    :accessor hdf-table-chain-active-file
    :documentation "hdf-file currently opened by the active hdf-table.")
   (file-paths
    :initform ()
    :initarg :file-paths
    :accessor hdf-table-chain-file-paths
    :documentation "List of file-paths which contain one hdf-table
    each")
   (table-lengths
    :initform ()
    :initarg :table-lengths
    :accessor hdf-table-chain-table-lengths
    :documentation "Length (in indices) of each table")
   (table-index-offsets
    :initform ()
    :initarg :index-offsets
    :accessor hdf-table-chain-table-index-offsets
    :documentation "Start indexes for each table")
   (current-table-index
    :initform -1
    :accessor hdf-table-chain-current-table-index
    :type integer
    :documentation "index for in-memory hdf-table in (virtual) list of tables")
   (current-table-start
    :initform -1
    :accessor hdf-table-chain-current-table-start
    :type integer
    :documentation "starting row index for the current table")
   (current-table-end
    :initform -1
    :accessor hdf-table-chain-current-table-end
    :type integer
    :documentation "last row index for the current table")
   (table-index-binary-tree
    :initform ()
    :initarg :binary-tree
    :accessor hdf-table-chain-binary-tree
    :documentation "binary search tree storing the indeces for
    efficient searching")
   (read-row-index
    :initarg :read-row-index
    :initform -1
    :accessor hdf-table-chain-read-row-index
    :documentation "Index to row which should be sequentually read
    next")
   (nrows
    :initarg :nrows
    :initform nil
    :accessor hdf-table-chain-nrows
    :documentation "number of rows in hdf-table")))

;; initialization:

;; For each path, read the dataset and get the number of entries in
;; each.  Use this to create the offset list.  Use the offset list to
;; create the hdf-table-chain-binary-tree.  Use this to create
(defun open-hdf-table-chain (filename-list dataset-path)
  "Creates and initializes hdf-table-chain given the filename-list and
dataset-path"
  (labels
      ((get-nrows (filename dset-path)
         (with-open-hdf-file (file
                              filename
                              :direction :input)
           (let* ((dataset (h5dopen2 file dset-path +H5P-DEFAULT+))
                  (dataspace (h5dget-space dataset))
                  (result
                   (with-foreign-objects ((space-dim 'hsize-t 1)
                                          (space-max-dim 'hsize-t 1))
                     (h5sget-simple-extent-dims
                      dataspace
                      space-dim
                      space-max-dim)
                     (mem-aref space-dim 'hsize-t 0))))
             (h5sclose dataspace)
             (h5dclose dataset)
             result)))
       (get-column-names (filename dataset-path)
	 (with-open-hdf-file (hdf-file
			      filename
			      :direction :input)
	   (let* ((hdf-table
                   (open-hdf-table hdf-file dataset-path))
		  (result (table-column-names hdf-table)))
	     (table-close hdf-table)
	     result)))
       (get-column-specs (filename dataset-path)
	 (with-open-hdf-file (hdf-file
			      filename
			      :direction :input)
	   (let* ((hdf-table
                   (open-hdf-table hdf-file dataset-path))
		  (result (typed-table-column-specs hdf-table)))
	     (table-close hdf-table)
	     result))))
    (let* ((file-nrows (mapcar
                        #'(lambda (filename)
                            (get-nrows filename dataset-path))
                        filename-list))
	   (offsets (make-offsets file-nrows))
	   (index-binary-tree
	    (make-index-binary-tree offsets))
           (table
            (make-instance 'hdf-table-chain
                           :column-names (get-column-names
                                          (first filename-list)
                                          dataset-path)
                           ;; :column-specs (get-column-specs
                           ;;                (first filename-list)
                           ;;                dataset-path)
                           :dataset-path dataset-path
                           :file-paths filename-list
                           :index-offsets offsets
                           :table-lengths file-nrows
                           :binary-tree index-binary-tree
                           :nrows (reduce #'+ file-nrows))))
      (let* ((current-table (hdf-table-chain-active-table table))
             (table-index 0)
             (filename (elt (hdf-table-chain-file-paths table)
                            table-index))
             (dataset-path (hdf-table-chain-dataset-path table)))
        (setf (hdf-table-chain-active-file table)
              (open-hdf-file filename :direction :input))
        (setf (hdf-table-chain-active-table table)
              (open-hdf-table (hdf-table-chain-active-file table)
                              dataset-path))
        (setf (hdf-table-chain-current-table-start table)
              (elt (hdf-table-chain-table-index-offsets table)
                   table-index))
        (setf (hdf-table-chain-current-table-end table)
              (+ (the integer
                      (elt (hdf-table-chain-table-lengths table)
                           table-index))
                 (hdf-table-chain-current-table-start table)
                 -1))
        table))))

;; finalization:
(defmethod table-close ((table-chain hdf-table-chain))
  "Closes the active hdf-table & file"
  (with-accessors ((active-table hdf-table-chain-active-table)
		   (active-file hdf-table-chain-active-file))
      table-chain
    (table-close active-table)
    (close-hdf-file active-file)))

;; table-get-field:
;; Procedure is to first check if index is within bounds.  If so,
;; create table-index from raw index and call table-get-field on
;; the active table.  If not, get table index from the raw index, get
;; the file path using this index, and read the appropriate table into
;; memory using open-hdf-table with the file path and dataset
;; path.  Then store the appropriate current-table-start/end and
;; change the table index to match the new in-memory table.  This
;; should be done before the call to table-get-field on the
;; active table.

(defmethod table-get-field ((table hdf-table-chain) column-symbol)
  (with-accessors ((active-table hdf-table-chain-active-table))
      table
    (table-get-field active-table column-symbol)))

(defmethod table-load-next-row ((table hdf-table-chain))
  (with-accessors ((row-number hdf-table-chain-read-row-index))
      table
    (incf row-number)
    (let* ((current-table-start (hdf-table-chain-current-table-start
                                 table))
           (current-table-end (hdf-table-chain-current-table-end
                               table)))
      (declare (integer current-table-start current-table-end))
      (if (<= current-table-start
              row-number
              current-table-end)
          (table-load-next-row (hdf-table-chain-active-table
                                table))
          (let* ((current-table (hdf-table-chain-active-table table))
                 (table-index (get-tree-index
                               (hdf-table-chain-binary-tree table)
                               row-number))
                 (filename (elt (hdf-table-chain-file-paths table)
                                table-index))
                 (dataset-path (hdf-table-chain-dataset-path table)))
            ;; free resources from previous table:
            (when current-table
              (table-close current-table)
              (close-hdf-file (hdf-table-chain-active-file table)))
            ;; establish next table:
            (setf (hdf-table-chain-active-file table)
                  (open-hdf-file filename :direction :input))
            (setf (hdf-table-chain-active-table table)
                  (open-hdf-table (hdf-table-chain-active-file table)
                                  dataset-path))
            (setf (hdf-table-chain-current-table-start table)
                  (elt (hdf-table-chain-table-index-offsets table)
                       table-index))
            (setf (hdf-table-chain-current-table-end table)
                  (+ (the integer
                          (elt (hdf-table-chain-table-lengths table)
                               table-index))
                     (hdf-table-chain-current-table-start table)
                     -1))
            (table-load-next-row
             (hdf-table-chain-active-table table)))))))

;;; Behind the scenes functions:

(defun make-index-binary-tree (offsets)
  (let ((offsets-copy (copy-list offsets)))
    (make-balanced-tree
     (zip (sort offsets-copy #'<)
	  (range 0 (1- (length offsets-copy))))
     :test #'equal
     :sorted t
     :key #'car)))

;; if this returns nil, the table index is 0
(defun get-tree-index (binary-tree row-index)
  (declare (integer row-index))
  (labels
      ((get-tree-index-worker (binary-tree row-index lastright)
         (declare (integer row-index))
	 (if binary-tree
	     (let* ((node-value (node-value binary-tree))
		    (node-start-index (car node-value)))
               (declare (integer node-start-index))
	       (cond
		 ((< row-index node-start-index)
		  (get-tree-index-worker
                   (node-left-child binary-tree)
                   row-index
                   lastright))
		 ((= row-index node-start-index)
		  (cdr node-value))
		 (t
		  (get-tree-index-worker
                   (node-right-child binary-tree)
                   row-index
                   (cdr node-value)))))
	     (if lastright
		 lastright
		 0))))
    (get-tree-index-worker binary-tree
			   row-index
			   nil)))
