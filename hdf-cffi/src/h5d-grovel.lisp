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

(cenum h5d-layout-t
       ((:H5D-LAYOUT-ERROR "H5D_LAYOUT_ERROR"))
       ((:H5D-COMPACT      "H5D_COMPACT"))
       ((:H5D-CONTIGUOUS   "H5D_CONTIGUOUS"))
       ((:H5D-CHUNKED      "H5D_CHUNKED"))
       ((:H5D-NLAYOUTS     "H5D_NLAYOUTS")))

(cenum h5d-alloc-time-t
       ((:H5D-ALLOC-TIME-ERROR   "H5D_ALLOC_TIME_ERROR"))
       ((:H5D-ALLOC-TIME-DEFAULT "H5D_ALLOC_TIME_DEFAULT"))
       ((:H5D-ALLOC-TIME-EARLY   "H5D_ALLOC_TIME_EARLY"))
       ((:H5D-ALLOC-TIME-LATE    "H5D_ALLOC_TIME_LATE"))
       ((:H5D-ALLOC-TIME-INCR    "H5D_ALLOC_TIME_INCR")))

(cenum h5d-space-status-t
       ((:H5D-SPACE-STATUS-ERROR          "H5D_SPACE_STATUS_ERROR"))
       ((:H5D-SPACE-STATUS-NOT-ALLOCATED  "H5D_SPACE_STATUS_NOT_ALLOCATED"))
       ((:H5D-SPACE-STATUS-PART-ALLOCATED "H5D_SPACE_STATUS_PART_ALLOCATED"))
       ((:H5D-SPACE-STATUS-ALLOCATED      "H5D_SPACE_STATUS_ALLOCATED")))

(cenum h5d-fill-time-t
       ((:H5D-FILL-TIME-ERROR "H5D_FILL_TIME_ERROR"))
       ((:H5D-FILL-TIME-ALLOC "H5D_FILL_TIME_ALLOC"))
       ((:H5D-FILL-TIME-NEVER "H5D_FILL_TIME_NEVER"))
       ((:H5D-FILL-TIME-IFSET "H5D_FILL_TIME_IFSET")))

(cenum h5d-fill-value-t
       ((:H5D-FILL-VALUE-ERROR        "H5D_FILL_VALUE_ERROR"))
       ((:H5D-FILL-VALUE-UNDEFINED    "H5D_FILL_VALUE_UNDEFINED"))
       ((:H5D-FILL-VALUE-DEFAULT      "H5D_FILL_VALUE_DEFAULT"))
       ((:H5D-FILL-VALUE-USER-DEFINED "H5D_FILL_VALUE_USER_DEFINED")))
