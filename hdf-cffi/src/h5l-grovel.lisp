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

(constant (+H5L-MAX-LINK-NAME-LEN+ "H5L_MAX_LINK_NAME_LEN"))

(constant (+H5L-SAME-LOC+ "H5L_SAME_LOC"))

(constant (+H5L-LINK-CLASS-T-VERS+ "H5L_LINK_CLASS_T_VERS"))

(cenum h5l-type-t
       ((:H5L-TYPE-ERROR    "H5L_TYPE_ERROR"))
       ((:H5L-TYPE-HARD     "H5L_TYPE_HARD"))
       ((:H5L-TYPE-SOFT     "H5L_TYPE_SOFT"))
       ((:H5L-TYPE-EXTERNAL "H5L_TYPE_EXTERNAL"))
       ((:H5L-TYPE-MAX      "H5L_TYPE_MAX")))

(cstruct h5l-class-t "H5L_class_t"
         (version "version"         :type :int)
         (id "id"                   :type h5l-type-t)
         (comment "comment"         :type (:pointer :char))
         (create-func "create_func" :type :pointer)
         (move-func "move_func"     :type :pointer)
         (copy-func "copy_func"     :type :pointer)
         (trav-func "trav_func"     :type :pointer)
         (del-func "del_func"       :type :pointer)
         (query-func "query_func"   :type :pointer))

(constant (+H5L-TYPE-BUILTIN-MAX+ "H5L_TYPE_BUILTIN_MAX"))
(constant (+H5L-TYPE-UD-MIN+ "H5L_TYPE_UD_MIN"))
