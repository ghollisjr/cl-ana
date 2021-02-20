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

(constant (+H5F-ACC-RDONLY+  "H5F_ACC_RDONLY"))
(constant (+H5F-ACC-RDWR+    "H5F_ACC_RDWR"))
(constant (+H5F-ACC-TRUNC+   "H5F_ACC_TRUNC"))
(constant (+H5F-ACC-EXCL+    "H5F_ACC_EXCL"))
(constant (+H5F-ACC-CREAT+   "H5F_ACC_CREAT"))
(constant (+H5F-ACC-DEFAULT+ "H5F_ACC_DEFAULT"))

(constant (+H5F-OBJ-FILE+     "H5F_OBJ_FILE"))
(constant (+H5F-OBJ-DATASET+  "H5F_OBJ_DATASET"))
(constant (+H5F-OBJ-GROUP+    "H5F_OBJ_GROUP"))
(constant (+H5F-OBJ-DATATYPE+ "H5F_OBJ_DATATYPE"))
(constant (+H5F-OBJ-ATTR+     "H5F_OBJ_ATTR"))
(constant (+H5F-OBJ-ALL+      "H5F_OBJ_ALL"))
(constant (+H5F-OBJ-LOCAL+    "H5F_OBJ_LOCAL"))

(cenum h5f-scope-t
       ((:H5F-SCOPE-LOCAL  "H5F_SCOPE_LOCAL"))
       ((:H5F-SCOPE-GLOBAL "H5F_SCOPE_GLOBAL")))

(constant (+H5F-UNLIMITED+ "H5F_UNLIMITED"))

(cenum h5f-close-degree-t
       ((:H5F-CLOSE-DEFAULT "H5F_CLOSE_DEFAULT"))
       ((:H5F-CLOSE-WEAK    "H5F_CLOSE_WEAK"))
       ((:H5F-CLOSE-SEMI    "H5F_CLOSE_SEMI"))
       ((:H5F-CLOSE-STRONG  "H5F_CLOSE_STRONG")))

(cenum h5f-mem-t
       ((:H5FD-MEM-NOLIST  "H5FD_MEM_NOLIST"))
       ((:H5FD-MEM-DEFAULT "H5FD_MEM_DEFAULT"))
       ((:H5FD-MEM-SUPER   "H5FD_MEM_SUPER"))
       ((:H5FD-MEM-BTREE   "H5FD_MEM_BTREE"))
       ((:H5FD-MEM-DRAW    "H5FD_MEM_DRAW"))
       ((:H5FD-MEM-GHEAP   "H5FD_MEM_GHEAP"))
       ((:H5FD-MEM-LHEAP   "H5FD_MEM_LHEAP"))
       ((:H5FD-MEM-OHDR    "H5FD_MEM_OHDR"))
       ((:H5FD-MEM-NTYPES  "H5FD_MEM_NTYPES")))

(cenum h5f-libver-t
       ((:H5F-LIBVER-EARLIEST "H5F_LIBVER_EARLIEST"))
       ((:H5F-LIBVER-LATEST   "H5F_LIBVER_LATEST")))

(constant (+H5F-LIBVER-18+ "H5F_LIBVER_18"))
