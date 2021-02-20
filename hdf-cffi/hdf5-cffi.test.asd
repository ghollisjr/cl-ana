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

(defsystem hdf5-cffi.test
  :serial t
  :description "hdf5-cffi is a CFFI wrapper for the HDF5 library."
  :version "1.8.18"
  :author "Gerd Heber <gheber@hdfgroup.org>"
  :license "BSD"
  :defsystem-depends-on (:cffi-grovel)
  :depends-on (:cffi :hdf5-cffi :hdf5-cffi.examples :fiveam)
  :components ((:file "t/test"))
  :perform (test-op :after (op c)
                    (eval (read-from-string "(5am:run! :hdf5-cffi)"))))


