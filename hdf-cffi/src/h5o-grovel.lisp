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

(constant (+H5O-COPY-SHALLOW-HIERARCHY-FLAG+     "H5O_COPY_SHALLOW_HIERARCHY_FLAG"))
(constant (+H5O-COPY-EXPAND-SOFT-LINK-FLAG+      "H5O_COPY_EXPAND_SOFT_LINK_FLAG"))
(constant (+H5O-COPY-EXPAND-EXT-LINK-FLAG+       "H5O_COPY_EXPAND_EXT_LINK_FLAG"))
(constant (+H5O-COPY-EXPAND-REFERENCE-FLAG+      "H5O_COPY_EXPAND_REFERENCE_FLAG"))
(constant (+H5O-COPY-WITHOUT-ATTR-FLAG+          "H5O_COPY_WITHOUT_ATTR_FLAG"))
(constant (+H5O-COPY-PRESERVE-NULL-FLAG+         "H5O_COPY_PRESERVE_NULL_FLAG"))
(constant (+H5O-COPY-MERGE-COMMITTED-DTYPE-FLAG+ "H5O_COPY_MERGE_COMMITTED_DTYPE_FLAG"))
(constant (+H5O-COPY-ALL+                        "H5O_COPY_ALL"))

(constant (+H5O-SHMESG-NONE-FLAG+     "H5O_SHMESG_NONE_FLAG"))
(constant (+H5O-SHMESG-SDSPACE-FLAG+  "H5O_SHMESG_SDSPACE_FLAG"))
(constant (+H5O-SHMESG-DTYPE-FLAG+    "H5O_SHMESG_DTYPE_FLAG"))
(constant (+H5O-SHMESG-FILL-FLAG+     "H5O_SHMESG_FILL_FLAG"))
(constant (+H5O-SHMESG-PLINE-FLAG+    "H5O_SHMESG_PLINE_FLAG"))
(constant (+H5O-SHMESG-ATTR-FLAG+     "H5O_SHMESG_ATTR_FLAG"))
(constant (+H5O-SHMESG-ALL-FLAG+      "H5O_SHMESG_ALL_FLAG"))
(constant (+H5O-SHMESG-MAX-NINDEXES+  "H5O_SHMESG_MAX_NINDEXES"))
(constant (+H5O-SHMESG-MAX-LIST-SIZE+ "H5O_SHMESG_MAX_LIST_SIZE"))

(constant (+H5O-HDR-CHUNK0-SIZE+             "H5O_HDR_CHUNK0_SIZE"))
(constant (+H5O-HDR-ATTR-CRT-ORDER_TRACKED+  "H5O_HDR_ATTR_CRT_ORDER_TRACKED"))
(constant (+H5O-HDR-ATTR-CRT-ORDER_INDEXED+  "H5O_HDR_ATTR_CRT_ORDER_INDEXED"))
(constant (+H5O-HDR-ATTR-STORE-PHASE-CHANGE+ "H5O_HDR_ATTR_STORE_PHASE_CHANGE"))
(constant (+H5O-HDR-STORE-TIMES+             "H5O_HDR_STORE_TIMES"))
(constant (+H5O-HDR-ALL-FLAGS+               "H5O_HDR_ALL_FLAGS"))

(cenum h5o-type-t
       ((:H5O-TYPE-UNKNOWN        "H5O_TYPE_UNKNOWN"))
       ((:H5O-TYPE-GROUP          "H5O_TYPE_GROUP"))
       ((:H5O-TYPE-DATASET        "H5O_TYPE_DATASET"))
       ((:H5O-TYPE-NAMED-DATATYPE "H5O_TYPE_NAMED_DATATYPE"))
       ((:H5O-TYPE-NTYPES         "H5O_TYPE_NTYPES")))

(ctype h5o-msg-crt-idx-t "H5O_msg_crt_idx_t")
