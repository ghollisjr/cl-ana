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

(cenum h5g-storage-type-t
       ((:H5G-STORAGE-TYPE-UNKNOWN      "H5G_STORAGE_TYPE_UNKNOWN"))
       ((:H5G-STORAGE-TYPE-SYMBOL-TABLE "H5G_STORAGE_TYPE_SYMBOL_TABLE"))
       ((:H5G-STORAGE-TYPE-COMPACT      "H5G_STORAGE_TYPE_COMPACT"))
       ((:H5G-STORAGE-TYPE-DENSE        "H5G_STORAGE_TYPE_DENSE")))

(cstruct h5g-info-t "H5G_info_t"
         (storage-type "storage_type" :type h5g-storage-type-t)
         (nlinks       "nlinks"       :type hsize-t)
         (max-corder   "max_corder"   :type :int64)
         (mounted      "mounted"      :type hbool-t))
