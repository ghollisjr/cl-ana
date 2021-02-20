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

(pkg-config-cflags "hdf5" :optional t)
(define "_H5private_H")    ; See GROVELLER-HACKING-NOTE.md for explanation
(include "hdf5.h")

(in-package #:hdf5)

(cenum h5pl-type-t
  ((:H5PL-TYPE-ERROR  "H5PL_TYPE_ERROR"))
  ((:H5PL-TYPE-FILTER "H5PL_TYPE_FILTER"))
  ((:H5PL-TYPE-NONE   "H5PL_TYPE_NONE")))

(constant (+H5PL-FILTER-PLUGIN+ "H5PL_FILTER_PLUGIN"))
(constant (+H5PL-ALL-PLUGIN+    "H5PL_ALL_PLUGIN"))
