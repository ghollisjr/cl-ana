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

(cenum h5i-type-t
       ((:H5I-UNINIT      "H5I_UNINIT"))
       ((:H5I-BADID       "H5I_BADID"))
       ((:H5I-FILE        "H5I_FILE"))
       ((:H5I-GROUP       "H5I_GROUP"))
       ((:H5I-DATATYPE    "H5I_DATATYPE"))
       ((:H5I-DATASPACE   "H5I_DATASPACE"))
       ((:H5I-DATASET     "H5I_DATASET"))
       ((:H5I-ATTR        "H5I_ATTR"))
       ;; ((:H5I-REFERENCE   "H5I_REFERENCE"))
       ((:H5I-VFL         "H5I_VFL"))
       ((:H5I-GENPROP-CLS "H5I_GENPROP_CLS"))
       ((:H5I-GENPROP-LST "H5I_GENPROP_LST"))
       ((:H5I-ERROR-CLASS "H5I_ERROR_CLASS"))
       ((:H5I-ERROR-MSG   "H5I_ERROR_MSG"))
       ((:H5I-ERROR-STACK "H5I_ERROR_STACK"))
       ((:H5I-NTYPES      "H5I_NTYPES")))

(ctype hid-t "hid_t")

(constant (+H5-SIZEOF-HID-T+ "H5_SIZEOF_HID_T"))

(constant (+H5I-INVALID-HID+ "H5I_INVALID_HID"))
