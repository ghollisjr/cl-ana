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

(constant (+H5-VERS-MAJOR+   "H5_VERS_MAJOR"))
(constant (+H5-VERS-MINOR+   "H5_VERS_MINOR"))
(constant (+H5-VERS-RELEASE+ "H5_VERS_RELEASE"))

(ctype herr-t   "herr_t")

(ctype hbool-t  "hbool_t")

(ctype htri-t   "htri_t")

(ctype ssize-t  "ssize_t")

(ctype hsize-t  "hsize_t")

(ctype hssize-t "hssize_t")

(ctype haddr-t  "haddr_t")

(constant (+HADDR-UNDEF+ "HADDR_UNDEF"))
(constant (+HADDR-MAX+   "HADDR_MAX"))

(cenum h5-iter-order-t
       ((:H5-ITER-UNKNOWN "H5_ITER_UNKNOWN"))
       ((:H5-ITER-INC     "H5_ITER_INC"))
       ((:H5-ITER-DEC     "H5_ITER_DEC"))
       ((:H5-ITER-NATIVE  "H5_ITER_NATIVE"))
       ((:H5-ITER-N       "H5_ITER_N")))

(constant (+H5-ITER-ERROR+ "H5_ITER_ERROR"))
(constant (+H5-ITER-CONT+  "H5_ITER_CONT"))
(constant (+H5-ITER-STOP+  "H5_ITER_STOP"))

(cenum h5-index-t
       ((:H5-INDEX-UNKNOWN   "H5_INDEX_UNKNOWN"))
       ((:H5-INDEX-NAME      "H5_INDEX_NAME"))
       ((:H5-INDEX-CRT-ORDER "H5_INDEX_CRT_ORDER"))
       ((:H5-INDEX-N         "H5_INDEX_N")))

(cstruct h5-ih-info-t "H5_ih_info_t"
         (index-size "index_size" :type hsize-t)
         (heap-size  "heap_size"  :type hsize-t))
