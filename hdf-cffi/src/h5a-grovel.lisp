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

(cstruct h5a-info-t "H5A_info_t"
         (corder-valid "corder_valid" :type hbool-t)
         (corder       "corder"       :type h5o-msg-crt-idx-t)
         (cset         "cset"         :type h5t-cset-t)
         (data-size    "data_size"    :type hsize-t))
