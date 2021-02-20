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

(cenum h5t-class-t
       ((:H5T-NO-CLASS  "H5T_NO_CLASS"))
       ((:H5T-INTEGER   "H5T_INTEGER"))
       ((:H5T-FLOAT     "H5T_FLOAT"))
       ((:H5T-TIME      "H5T_TIME"))
       ((:H5T-STRING    "H5T_STRING"))
       ((:H5T-BITFIELD  "H5T_BITFIELD"))
       ((:H5T-OPAQUE    "H5T_OPAQUE"))
       ((:H5T-COMPOUND  "H5T_COMPOUND"))
       ((:H5T-REFERENCE "H5T_REFERENCE"))
       ((:H5T-ENUM      "H5T_ENUM"))
       ((:H5T-VLEN      "H5T_VLEN"))
       ((:H5T-ARRAY     "H5T_ARRAY"))
       ((:H5T-NCLASSES  "H5T_NCLASSES")))

(cenum h5t-order-t
       ((:H5T-ORDER-ERROR "H5T_ORDER_ERROR"))
       ((:H5T-ORDER-LE    "H5T_ORDER_LE"))
       ((:H5T-ORDER-BE    "H5T_ORDER_BE"))
       ((:H5T-ORDER-VAX   "H5T_ORDER_VAX"))
       ((:H5T-ORDER-MIXED "H5T_ORDER_MIXED"))
       ((:H5T-ORDER-NONE  "H5T_ORDER_NONE")))

(cenum h5t-sign-t
       ((:H5T-SGN-ERROR "H5T_SGN_ERROR"))
       ((:H5T-SGN-NONE  "H5T_SGN_NONE"))
       ((:H5T-SGN-2     "H5T_SGN_2"))
       ((:H5T-NSGN      "H5T_NSGN")))

(cenum h5t-norm-t
       ((:H5T-NORM-ERROR   "H5T_NORM_ERROR"))
       ((:H5T-NORM-IMPLIED "H5T_NORM_IMPLIED"))
       ((:H5T-NORM-MSBSET  "H5T_NORM_MSBSET"))
       ((:H5T-NORM-NONE    "H5T_NORM_NONE")))

(cenum h5t-cset-t
       ((:H5T-CSET-ERROR       "H5T_CSET_ERROR"))
       ((:H5T-CSET-ASCII       "H5T_CSET_ASCII"))
       ((:H5T-CSET-UTF8        "H5T_CSET_UTF8"))
       ((:H5T-CSET-RESERVED-2  "H5T_CSET_RESERVED_2"))
       ((:H5T-CSET-RESERVED-3  "H5T_CSET_RESERVED_3"))
       ((:H5T-CSET-RESERVED-4  "H5T_CSET_RESERVED_4"))
       ((:H5T-CSET-RESERVED-5  "H5T_CSET_RESERVED_5"))
       ((:H5T-CSET-RESERVED-6  "H5T_CSET_RESERVED_6"))
       ((:H5T-CSET-RESERVED-7  "H5T_CSET_RESERVED_7"))
       ((:H5T-CSET-RESERVED-8  "H5T_CSET_RESERVED_8"))
       ((:H5T-CSET-RESERVED-9  "H5T_CSET_RESERVED_9"))
       ((:H5T-CSET-RESERVED-10 "H5T_CSET_RESERVED_10"))
       ((:H5T-CSET-RESERVED-11 "H5T_CSET_RESERVED_11"))
       ((:H5T-CSET-RESERVED-12 "H5T_CSET_RESERVED_12"))
       ((:H5T-CSET-RESERVED-13 "H5T_CSET_RESERVED_13"))
       ((:H5T-CSET-RESERVED-14 "H5T_CSET_RESERVED_14"))
       ((:H5T-CSET-RESERVED-15 "H5T_CSET_RESERVED_15")))

(constant (+H5T-NCSET+ "H5T_NCSET"))

(cenum h5t-str-t
       ((:H5T-STR-ERROR       "H5T_STR_ERROR"))
       ((:H5T-STR-NULLTERM    "H5T_STR_NULLTERM"))
       ((:H5T-STR-NULLPAD     "H5T_STR_NULLPAD"))
       ((:H5T-STR-SPACEPAD    "H5T_STR_SPACEPAD"))
       ((:H5T-STR-RESERVED-3  "H5T_STR_RESERVED_3"))
       ((:H5T-STR-RESERVED-4  "H5T_STR_RESERVED_4"))
       ((:H5T-STR-RESERVED-5  "H5T_STR_RESERVED_5"))
       ((:H5T-STR-RESERVED-6  "H5T_STR_RESERVED_6"))
       ((:H5T-STR-RESERVED-7  "H5T_STR_RESERVED_7"))
       ((:H5T-STR-RESERVED-8  "H5T_STR_RESERVED_8"))
       ((:H5T-STR-RESERVED-9  "H5T_STR_RESERVED_9"))
       ((:H5T-STR-RESERVED-10 "H5T_STR_RESERVED_10"))
       ((:H5T-STR-RESERVED-11 "H5T_STR_RESERVED_11"))
       ((:H5T-STR-RESERVED-12 "H5T_STR_RESERVED_12"))
       ((:H5T-STR-RESERVED-13 "H5T_STR_RESERVED_13"))
       ((:H5T-STR-RESERVED-14 "H5T_STR_RESERVED_14"))
       ((:H5T-STR-RESERVED-15 "H5T_STR_RESERVED_15")))

(constant (+H5T-NSTR+ "H5T_NSTR"))

(cenum h5t-pad-t
       ((:H5T-PAD-ERROR      "H5T_PAD_ERROR"))
       ((:H5T-PAD-ZERO       "H5T_PAD_ZERO"))
       ((:H5T-PAD-ONE        "H5T_PAD_ONE"))
       ((:H5T-PAD-BACKGROUND "H5T_PAD_BACKGROUND"))
       ((:H5T-NPAD           "H5T_NPAD")))

(cenum h5t-cmd-t
       ((:H5T-CONV-INIT "H5T_CONV_INIT"))
       ((:H5T-CONV-CONV "H5T_CONV_CONV"))
       ((:H5T-CONV-FREE "H5T_CONV_FREE")))

(cenum h5t-bkg-t
       ((:H5T-BKG-NO   "H5T_BKG_NO"))
       ((:H5T-BKG-TEMP "H5T_BKG_TEMP"))
       ((:H5T-BKG-YES  "H5T_BKG_YES")))

(cstruct h5t-cdata-t "H5T_cdata_t"
         (command  "command"  :type H5T-cmd-t)
         (need-bkg "need_bkg" :type H5T-bkg-t)
         (recalc   "recalc"   :type hbool-t)
         (priv     "priv"     :type :pointer))

(cenum h5t-pers-t
       ((:H5T-PERS-DONTCARE "H5T_PERS_DONTCARE"))
       ((:H5T-PERS-HARD     "H5T_PERS_HARD"))
       ((:H5T-PERS-SOFT     "H5T_PERS_SOFT")))

(cenum h5t-dir-t
       ((:H5T-DIR-DEFAULT "H5T_DIR_DEFAULT"))
       ((:H5T-DIR-ASCEND  "H5T_DIR_ASCEND"))
       ((:H5T-DIR-DESCEND "H5T_DIR_DESCEND")))

(cenum h5t-conv-except-t
       ((:H5T-CONV-EXCEPT-RANGE-HI  "H5T_CONV_EXCEPT_RANGE_HI"))
       ((:H5T-CONV-EXCEPT-RANGE-LOW "H5T_CONV_EXCEPT_RANGE_LOW"))
       ((:H5T-CONV-EXCEPT-PRECISION "H5T_CONV_EXCEPT_PRECISION"))
       ((:H5T-CONV-EXCEPT-TRUNCATE  "H5T_CONV_EXCEPT_TRUNCATE"))
       ((:H5T-CONV-EXCEPT-PINF      "H5T_CONV_EXCEPT_PINF"))
       ((:H5T-CONV-EXCEPT-NINF      "H5T_CONV_EXCEPT_NINF"))
       ((:H5T-CONV-EXCEPT-NAN       "H5T_CONV_EXCEPT_NAN")))

(cenum h5t-conv-ret-t
       ((:H5T-CONV-ABORT     "H5T_CONV_ABORT"))
       ((:H5T-CONV-UNHANDLED "H5T_CONV_UNHANDLED"))
       ((:H5T-CONV-HANDLED   "H5T_CONV_HANDLED")))

(cstruct hvl-t "hvl_t"
         (len "len" :type size-t)
         (p   "p"   :type :pointer))

(constant (+H5T-VARIABLE+ "H5T_VARIABLE"))

(constant (+H5T-OPAQUE-TAG-MAX+ "H5T_OPAQUE_TAG_MAX"))

(constant (+CHAR-MIN+ "CHAR_MIN"))
