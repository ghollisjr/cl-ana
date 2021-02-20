;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright by The HDF Group.
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.
;;;;
;;;; Note: This project has been modified and included as part of
;;;; cl-ana until bug fixes can be added upstream.

(defsystem cl-ana.hdf-cffi ; used to be hdf5-cffi
  :serial t
  :description "hdf5-cffi is a CFFI wrapper for the HDF5 library."
  :version "1.8.18"
  :author "Gerd Heber <gheber@hdfgroup.org>"
  :license "BSD"
  :defsystem-depends-on (:cffi-grovel)
  :depends-on (:cffi)
  :pathname "src/"
  :components
  ((:file "package")
   (:file "library")
   (:cffi-grovel-file "grovel")
   (:cffi-grovel-file "h5-grovel")
   (:file "h5")
   (:cffi-grovel-file "h5i-grovel")
   (:file "h5i")
   (:cffi-grovel-file "h5f-grovel")
   (:file "h5f")
   (:cffi-grovel-file "h5t-grovel")
   (:file "h5t")
   (:cffi-grovel-file "h5l-grovel")
   (:file "h5l")
   (:cffi-grovel-file "h5o-grovel")
   (:file "h5o")
   (:cffi-grovel-file "h5s-grovel")
   (:file "h5s")
   (:cffi-grovel-file "h5d-grovel")
   (:file "h5d")
   (:cffi-grovel-file "h5g-grovel")
   (:file "h5g")
   (:cffi-grovel-file "h5a-grovel")
   (:file "h5a")
   ;; (:cffi-grovel-file "h5pl-grovel")
   ;; (:file "h5pl")
   (:cffi-grovel-file "h5r-grovel")
   (:file "h5r")
   (:cffi-grovel-file "h5z-grovel")
   (:file "h5z")
   (:cffi-grovel-file "h5p-grovel")
   (:file "h5p"))
  :in-order-to ((test-op (test-op :hdf5-cffi.test))))
