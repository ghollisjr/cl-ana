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

(ctype h5z-filter-t "H5Z_filter_t")

(constant (+H5Z-FILTER-ERROR+       "H5Z_FILTER_ERROR"))
(constant (+H5Z-FILTER-NONE+        "H5Z_FILTER_NONE"))
(constant (+H5Z-FILTER-DEFLATE+     "H5Z_FILTER_DEFLATE"))
(constant (+H5Z-FILTER-SHUFFLE+     "H5Z_FILTER_SHUFFLE"))
(constant (+H5Z-FILTER-FLETCHER32+  "H5Z_FILTER_FLETCHER32"))
(constant (+H5Z-FILTER-SZIP+        "H5Z_FILTER_SZIP"))
(constant (+H5Z-FILTER-NBIT+        "H5Z_FILTER_NBIT"))
(constant (+H5Z-FILTER-SCALEOFFSET+ "H5Z_FILTER_SCALEOFFSET"))
(constant (+H5Z-FILTER-RESERVED+    "H5Z_FILTER_RESERVED"))

(constant (+H5Z-FILTER-MAX+ "H5Z_FILTER_MAX"))

(constant (+H5Z-FILTER-ALL+   "H5Z_FILTER_ALL"))
(constant (+H5Z-MAX-NFILTERS+ "H5Z_MAX_NFILTERS"))

(constant (+H5Z-FLAG-DEFMASK+   "H5Z_FLAG_DEFMASK"))
(constant (+H5Z-FLAG-MANDATORY+ "H5Z_FLAG_MANDATORY"))
(constant (+H5Z-FLAG-OPTIONAL+  "H5Z_FLAG_OPTIONAL"))

(constant (+H5Z-FLAG-INVMASK+  "H5Z_FLAG_INVMASK"))
(constant (+H5Z-FLAG-REVERSE+  "H5Z_FLAG_REVERSE"))
(constant (+H5Z-FLAG-SKIP-EDC+ "H5Z_FLAG_SKIP_EDC"))

(constant (+H5-SZIP-ALLOW-K13-OPTION-MASK+ "H5_SZIP_ALLOW_K13_OPTION_MASK"))
(constant (+H5-SZIP-CHIP-OPTION-MASK+      "H5_SZIP_CHIP_OPTION_MASK"))
(constant (+H5-SZIP-EC-OPTION-MASK+        "H5_SZIP_EC_OPTION_MASK"))
(constant (+H5-SZIP-NN-OPTION-MASK+        "H5_SZIP_NN_OPTION_MASK"))
(constant (+H5-SZIP-MAX-PIXELS-PER-BLOCK+  "H5_SZIP_MAX_PIXELS_PER_BLOCK"))

(constant (+H5Z-SHUFFLE-USER-NPARMS+  "H5Z_SHUFFLE_USER_NPARMS"))
(constant (+H5Z-SHUFFLE-TOTAL-NPARMS+ "H5Z_SHUFFLE_TOTAL_NPARMS"))

(constant (+H5Z-SZIP-USER-NPARMS+  "H5Z_SZIP_USER_NPARMS"))
(constant (+H5Z-SZIP-TOTAL-NPARMS+ "H5Z_SZIP_TOTAL_NPARMS"))
(constant (+H5Z-SZIP-PARM-MASK+    "H5Z_SZIP_PARM_MASK"))
(constant (+H5Z-SZIP-PARM-PPB+     "H5Z_SZIP_PARM_PPB"))
(constant (+H5Z-SZIP-PARM-BPP+     "H5Z_SZIP_PARM_BPP"))
(constant (+H5Z-SZIP-PARM-PPS+     "H5Z_SZIP_PARM_PPS"))

(constant (+H5Z-NBIT-USER-NPARMS+ "H5Z_NBIT_USER_NPARMS"))

(constant (+H5Z-SCALEOFFSET-USER-NPARMS+ "H5Z_SCALEOFFSET_USER_NPARMS"))

(constant (+H5Z-SO-INT-MINBITS-DEFAULT+ "H5Z_SO_INT_MINBITS_DEFAULT"))

(cenum h5z-so-scale-type-t
       ((:H5Z-SO-FLOAT-DSCALE "H5Z_SO_FLOAT_DSCALE"))
       ((:H5Z-SO-FLOAT-ESCALE "H5Z_SO_FLOAT_ESCALE"))
       ((:H5Z-SO-INT          "H5Z_SO_INT")))

(constant (+H5Z-CLASS-T-VERS+ "H5Z_CLASS_T_VERS"))

(cenum h5z-edc-t
       ((:H5Z-ERROR-EDC   "H5Z_ERROR_EDC"))
       ((:H5Z-DISABLE-EDC "H5Z_DISABLE_EDC"))
       ((:H5Z-ENABLE-EDC  "H5Z_ENABLE_EDC"))
       ((:H5Z-NO-EDC      "H5Z_NO_EDC")))

(constant (+H5Z-FILTER-CONFIG-ENCODE-ENABLED+ "H5Z_FILTER_CONFIG_ENCODE_ENABLED"))
(constant (+H5Z-FILTER-CONFIG-DECODE-ENABLED+ "H5Z_FILTER_CONFIG_DECODE_ENABLED"))
