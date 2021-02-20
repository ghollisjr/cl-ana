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

(constant (+H5S-ALL+ "H5S_ALL"))
(constant (+H5S-UNLIMITED+ "H5S_UNLIMITED"))

(constant (+H5S-MAX-RANK+ "H5S_MAX_RANK"))

(cenum h5s-class-t
       ((:H5S-NO-CLASS "H5S_NO_CLASS"))
       ((:H5S-SCALAR   "H5S_SCALAR"))
       ((:H5S-SIMPLE   "H5S_SIMPLE"))
       ((:H5S-NULL     "H5S_NULL")))

(cenum h5s-seloper-t
       ((:H5S-SELECT-NOOP    "H5S_SELECT_NOOP"))
       ((:H5S-SELECT-SET     "H5S_SELECT_SET"))
       ((:H5S-SELECT-OR      "H5S_SELECT_OR"))
       ((:H5S-SELECT-AND     "H5S_SELECT_AND"))
       ((:H5S-SELECT-XOR     "H5S_SELECT_XOR"))
       ((:H5S-SELECT-NOTB    "H5S_SELECT_NOTB"))
       ((:H5S-SELECT-NOTA    "H5S_SELECT_NOTA"))
       ((:H5S-SELECT-APPEND  "H5S_SELECT_APPEND"))
       ((:H5S-SELECT-PREPEND "H5S_SELECT_PREPEND"))
       ((:H5S-SELECT-INVALID "H5S_SELECT_INVALID")))

(cenum h5s-sel-type
       ((:H5S-SEL-ERROR      "H5S_SEL_ERROR"))
       ((:H5S-SEL-NONE       "H5S_SEL_NONE"))
       ((:H5S-SEL-POINTS     "H5S_SEL_POINTS"))
       ((:H5S-SEL-HYPERSLABS "H5S_SEL_HYPERSLABS"))
       ((:H5S-SEL-ALL        "H5S_SEL_ALL"))
       ((:H5S-SEL-N          "H5S_SEL_N")))
