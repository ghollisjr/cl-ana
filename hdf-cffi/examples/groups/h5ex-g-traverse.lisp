;;;; Copyright by The HDF Group.
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows a way to recursively traverse the file
;;; using H5Literate.  The method shown here guarantees that
;;; the recursion will not enter an infinite loop, but does
;;; not prevent objects from being visited more than once.
;;; The program prints the directory structure of the file
;;; specified in FILE.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5G/h5ex_g_traverse.c




(in-package :hdf5)

(defparameter *FILE* (namestring (merge-pathnames "h5ex_g_traverse.h5" *load-pathname*)))

;;; Define operator data structure type for H5Literate callback.
;;; During recursive iteration, these structures will form a
;;; linked list that can be searched for duplicate groups,
;;; preventing infinite recursion.

(cffi:defcstruct opdata
    (recurs :unsigned-int) ; Recursion level.  0=root
  (prev :pointer)        ; Pointer to previous opdata
  (addr haddr-t))        ; Group address

;;; Function to check for duplicate groups in a path.
;;;
;;; This function recursively searches the linked list of
;;; opdata structures for one whose address matches
;;; target_addr.  Returns T if a match is found, and NIL
;;; otherwise.

(defun group-check (opdata target-addr)
  (cffi:with-foreign-slots ((addr recurs prev) opdata (:struct opdata))
    (cond ((eql addr target-addr) t) ; Addresses match
          ((eql 0 recurs) nil)       ; Root group reached with no matches
          (t (group-check prev target-addr))))) ; examine the next node

;;; Operator function.  This function prints the name and type
;;; of the object passed to it.  If the object is a group, it
;;; is first checked against other groups in its path using
;;; the group_check function, then if it is not a duplicate,
;;; H5Literate is called for that group.  This guarantees that
;;; the program will not enter infinite recursion due to a
;;; circular path in the file.

(cffi:defcallback op-func herr-t
  ((loc-id        hid-t)
   (name          (:pointer :char))
   (info          (:pointer (:struct h5l-info-t)))
   (operator-data :pointer))
  (declare (ignore info))
  (cffi:with-foreign-objects ((infobuf '(:struct h5o-info-t) 1)
                              (nextod '(:struct opdata) 1))
    (let* ((return-val 0)
           (od.recurs (cffi:foreign-slot-value
                       operator-data '(:struct opdata) 'recurs))
           ;; Number of whitespaces to prepend to output
           (spaces (* 2 (1+ od.recurs)))
           (name-string (cffi:foreign-string-to-lisp name)))

      ;; Get type of the object and display its name and type.
      ;; The name of the object is passed to this function by
      ;; the Library.

      (h5oget-info-by-name loc-id name infobuf +H5P-DEFAULT+)
      (format t "~VA" spaces #\Space)

      (cffi:with-foreign-slots ((type addr) infobuf (:struct h5o-info-t))
        (let ((infobuf.type type)
              (infobuf.addr addr))
          (cond
            ((eql infobuf.type :H5O-TYPE-GROUP)
             (format t "Group: ~a {~%" name-string)
             (cond ((group-check operator-data infobuf.addr)
                    (format t "~VA  Warning: Loop detected!~%"
                            spaces #\Space))
                   (t (cffi:with-foreign-slots ((recurs prev addr)
                                                nextod (:struct opdata))
                        (setf recurs (1+ od.recurs)
                              prev operator-data
                              addr infobuf.addr))
                      (setq return-val (h5literate-by-name
                                        loc-id name :H5-INDEX-NAME
                                        :H5-ITER-NATIVE +NULL+
                                        (cffi:callback op-func)
                                        nextod +H5P-DEFAULT+))))
             (format t "~VA}~%" spaces #\Space))
            ((eql infobuf.type :H5O-TYPE-DATASET)
             (format t "Dataset: ~a~%" name-string))
            ((eql infobuf.type :H5O-TYPE-NAMED-DATATYPE)
             (format t "Datatype: ~a~%" name-string))
            (t (format t "Unknown: ~a~%" name-string)))))
      return-val)))


(cffi:with-foreign-objects ((infobuf '(:struct h5o-info-t) 1)
                            (od '(:struct opdata) 1))

  ;; Open file and initialize the operator data structure.

  (let* ((fapl (h5pcreate +H5P-FILE-ACCESS+))
         (file (prog2 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
                   (h5fopen *FILE* +H5F-ACC-RDONLY+ fapl))))
    (unwind-protect
         (progn
           (h5oget-info file infobuf)
           (cffi:with-foreign-slots ((recurs prev addr) od (:struct opdata))
             (setf recurs 0
                   prev +NULL+
                   addr (cffi:foreign-slot-value
                         infobuf '(:struct h5o-info-t) 'addr)))
           (format t "/ {~%")
           (h5literate file :H5-INDEX-NAME :H5-ITER-NATIVE
                       +NULL+ (cffi:callback op-func) od)
           (format t "}~%"))
      (h5ex:close-handles (list file fapl)))))
