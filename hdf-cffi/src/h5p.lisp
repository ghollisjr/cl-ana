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

(in-package #:hdf5)

(defcfun "H5Pclose" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-Close"
  (plist hid-t))

(defcfun "H5Pcreate" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-Create"
  (cls-id hid-t))

(defcfun "H5Pcopy" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-Copy"
  (plist hid-t))

(defcfun "H5Pget_char_encoding" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetCharEncoding"
  (plist-id hid-t)
  (encoding (:pointer h5t-cset-t)))

(defcfun "H5Pget_chunk" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetChunk"
  (plist     hid-t)
  (max-ndims :int)
  (dims      (:pointer hsize-t)))

(defcfun "H5Pget_class" hid-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetClass"
  (plist hid-t))

(if (foreign-symbol-pointer "H5Pget_core_write_tracking")
    (defcfun "H5Pget_core_write_tracking" herr-t
      "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetCoreWriteTracking"
      (fapl-id    hid-t)
      (is-enabled (:pointer hbool-t))
      (page-size  (:pointer size-t))))

(defcfun "H5Pget_create_intermediate_group" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetCreateIntermediateGroup"
  (lcpl-d             hid-t)
  (crt-intermed-group (:pointer :uint)))

(defcfun "H5Pget_external" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetExternal"
  (plist     hid-t)
  (idx       :uint)
  (name-size size-t)
  (name      (:pointer :char))
  (offset    (:pointer off-t))
  (size      (:pointer hsize-t)))

(defcfun "H5Pget_external_count" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetExternalCount"
  (plist hid-t))

(defcfun "H5Pget_fapl_core" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetFaplCore"
  (fapl-id hid-t)
  (increment (:pointer size-t))
  (backing-store (:pointer hbool-t)))

(defcfun "H5Pget_fclose_degree" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetFcloseDegree"
  (fapl-id   hid-t)
  (fc-degree (:pointer h5f-close-degree-t)))

(defcfun "H5Pget_file_image" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetFileImage"
  (fapl-id     hid-t)
  (buf-ptr-ptr :pointer)
  (buf-len     (:pointer size-t)))

(defcfun "H5Pget_fill_value" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetFillValue"
  (plist-id hid-t)
  (type-id  hid-t)
  (value    :pointer))

(defcfun "H5Pget_filter2" h5z-filter-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetFilter2"
  (plist-id      hid-t)
  (idx           :unsigned-int)
  (flags         (:pointer :unsigned-int))
  (cd-nelmts     (:pointer size-t))
  (cd-values     (:pointer :unsigned-int))
  (namelen       size-t)
  (name          (:pointer :char))
  (filter-config (:pointer :unsigned-int)))

(defcfun "H5Pget_layout" h5d-layout-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetLayout"
  (plist hid-t))

(defcfun "H5Pget_libver_bounds" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetLibverBounds"
  (fapl-id     hid-t)
  (libver-low  (:pointer h5f-libver-t))
  (libver-high (:pointer h5f-libver-t)))

(defcfun "H5Pget_nfilters" :int
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetNFilters"
  (plist hid-t))

(defcfun "H5Pget_sizes" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetSizes"
  (plist       hid-t)
  (sizeof-addr (:pointer size-t))
  (sizeof-size (:pointer size-t)))

(defcfun "H5Pget_userblock" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetUserblock"
  (plist hid-t)
  (size  (:pointer hsize-t)))

(defcfun "H5Pget_version" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-GetVersion"
  (plist    hid-t)
  (super    (:pointer :uint))
  (freelist (:pointer :uint))
  (stab     (:pointer :uint))
  (shhdr    (:pointer :uint)))

(defcfun "H5Pset_alloc_time" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetAllocTime"
  (plist-id hid-t)
  (alloc-time h5d-alloc-time-t))

(defcfun "H5Pset_char_encoding" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetCharEncoding"
  (plist-id hid-t)
  (encoding h5t-cset-t))

(defcfun "H5Pset_chunk" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetChunk"
  (plist hid-t)
  (ndims :int)
  (dim   (:pointer hsize-t)))

(if (foreign-symbol-pointer "H5Pset_core_write_tracking")
    (defcfun "H5Pset_core_write_tracking" herr-t
      "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetCoreWriteTracking"
      (fapl-id    hid-t)
      (is-enabled hbool-t)
      (page-size  size-t)))

(defcfun "H5Pset_create_intermediate_group" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetCreateIntermediateGroup"
  (lcpl-d             hid-t)
  (crt-intermed-group :uint))

(defcfun "H5Pset_data_transform" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetDataTransform"
  (plist-id   hid-t)
  (expression (:pointer :char)))

(defcfun "H5Pset_deflate" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetDeflate"
  (plist-id hid-t)
  (level    :uint))

(defcfun "H5Pset_external" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetExternal"
  (plist  hid-t)
  (name   :string)
  (offset off-t)
  (size   hsize-t))

(defcfun "H5Pset_fapl_core" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetFaplCore"
  (fapl-id       hid-t)
  (increment     size-t)
  (backing-store hbool-t))

(defcfun "H5Pset_fclose_degree" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetFcloseDegree"
  (fapl-id   hid-t)
  (fc-degree h5f-close-degree-t))

(defcfun "H5Pset_file_image" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetFileImage"
  (fapl-id hid-t)
  (buf-ptr :pointer)
  (buf-len size-t))

(defcfun "H5Pset_fill_value" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetFillValue"
  (plist-id hid-t)
  (type-id  hid-t)
  (value    :pointer))

(defcfun "H5Pset_fletcher32" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetFletcher32"
  (plist-id hid-t))

(defcfun "H5Pset_layout" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetLayout"
  (plist  hid-t)
  (layout h5d-layout-t))

(defcfun "H5Pset_libver_bounds" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetLibverBounds"
  (fapl-id     hid-t)
  (libver-low  h5f-libver-t)
  (libver-high h5f-libver-t))

(defcfun "H5Pset_link_creation_order" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetLinkCreationOrder"
  (gcpl-id         hid-t)
  (crt-order-flags :unsigned-int))

(defcfun "H5Pset_link_phase_change" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetLinkPhaseChange"
  (gcpl-id     hid-t)
  (max-compact :unsigned-int)
  (min-dense   :unsigned-int))

(defcfun "H5Pset_nbit" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetNbit"
  (plist-id hid-t))

(defcfun "H5Pset_scaleoffset" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetScaleoffset"
  (plist-id     hid-t)
  (scale-type   h5z-so-scale-type-t)
  (scale-factor :int))

(defcfun "H5Pset_shuffle" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetShuffle"
  (plist-id hid-t))

(if (foreign-symbol-pointer "H5Pset_szip")
    (defcfun "H5Pset_szip" herr-t
      "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetSzip"
      (plist            hid-t)
      (options-mask     :unsigned-int)
      (pixels-per-block :unsigned-int)))

(defcfun "H5Pset_userblock" herr-t
  "http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetUserblock"
  (plist hid-t)
  (size  hsize-t))



;;

;; /*
;;  * The library's property list classes
;;  */

(defcvar (#.(lispify "H5P_ROOT" 'constant)             "H5P_CLS_ROOT_ID_g")             hid-t)
(defcvar (#.(lispify "H5P_OBJECT_CREATE" 'constant)    "H5P_CLS_OBJECT_CREATE_ID_g")    hid-t)
(defcvar (#.(lispify "H5P_FILE_CREATE" 'constant)      "H5P_CLS_FILE_CREATE_ID_g")      hid-t)
(defcvar (#.(lispify "H5P_FILE_ACCESS" 'constant)      "H5P_CLS_FILE_ACCESS_ID_g")      hid-t)
(defcvar (#.(lispify "H5P_DATASET_CREATE" 'constant)   "H5P_CLS_DATASET_CREATE_ID_g")   hid-t)
(defcvar (#.(lispify "H5P_DATASET_ACCESS" 'constant)   "H5P_CLS_DATASET_ACCESS_ID_g")   hid-t)
(defcvar (#.(lispify "H5P_DATASET_XFER" 'constant)     "H5P_CLS_DATASET_XFER_ID_g")     hid-t)
(defcvar (#.(lispify "H5P_FILE_MOUNT" 'constant)       "H5P_CLS_FILE_MOUNT_ID_g")       hid-t)
(defcvar (#.(lispify "H5P_GROUP_CREATE" 'constant)     "H5P_CLS_GROUP_CREATE_ID_g")     hid-t)
(defcvar (#.(lispify "H5P_GROUP_ACCESS" 'constant)     "H5P_CLS_GROUP_ACCESS_ID_g")     hid-t)
(defcvar (#.(lispify "H5P_DATATYPE_CREATE" 'constant)  "H5P_CLS_DATATYPE_CREATE_ID_g")  hid-t)
(defcvar (#.(lispify "H5P_DATATYPE_ACCESS" 'constant)  "H5P_CLS_DATATYPE_ACCESS_ID_g")  hid-t)
(defcvar (#.(lispify "H5P_STRING_CREATE" 'constant)    "H5P_CLS_STRING_CREATE_ID_g")    hid-t)
(defcvar (#.(lispify "H5P_ATTRIBUTE_CREATE" 'constant) "H5P_CLS_ATTRIBUTE_CREATE_ID_g") hid-t)
(defcvar (#.(lispify "H5P_ATTRIBUTE_ACCESS" 'constant) "H5P_CLS_ATTRIBUTE_ACCESS_ID_g") hid-t)
(defcvar (#.(lispify "H5P_OBJECT_COPY" 'constant)      "H5P_CLS_OBJECT_COPY_ID_g")      hid-t)
(defcvar (#.(lispify "H5P_LINK_CREATE" 'constant)      "H5P_CLS_LINK_CREATE_ID_g")      hid-t)
(defcvar (#.(lispify "H5P_LINK_ACCESS" 'constant)      "H5P_CLS_LINK_ACCESS_ID_g")      hid-t)

;; /*
;;  * The library's default property lists
;;  */
(defcvar (#.(lispify "H5P_FILE_CREATE_DEFAULT" 'constant)      "H5P_LST_FILE_CREATE_ID_g")      hid-t)
(defcvar (#.(lispify "H5P_FILE_ACCESS_DEFAULT" 'constant)      "H5P_LST_FILE_ACCESS_ID_g")      hid-t)
(defcvar (#.(lispify "H5P_DATASET_CREATE_DEFAULT" 'constant)   "H5P_LST_DATASET_CREATE_ID_g")   hid-t)
(defcvar (#.(lispify "H5P_DATASET_ACCESS_DEFAULT" 'constant)   "H5P_LST_DATASET_ACCESS_ID_g")   hid-t)
(defcvar (#.(lispify "H5P_DATASET_XFER_DEFAULT" 'constant)     "H5P_LST_DATASET_XFER_ID_g")     hid-t)
(defcvar (#.(lispify "H5P_FILE_MOUNT_DEFAULT" 'constant)       "H5P_LST_FILE_MOUNT_ID_g")       hid-t)
(defcvar (#.(lispify "H5P_GROUP_CREATE_DEFAULT" 'constant)     "H5P_LST_GROUP_CREATE_ID_g")     hid-t)
(defcvar (#.(lispify "H5P_GROUP_ACCESS_DEFAULT" 'constant)     "H5P_LST_GROUP_ACCESS_ID_g")     hid-t)
(defcvar (#.(lispify "H5P_DATATYPE_CREATE_DEFAULT" 'constant)  "H5P_LST_DATATYPE_CREATE_ID_g")  hid-t)
(defcvar (#.(lispify "H5P_DATATYPE_ACCESS_DEFAULT" 'constant)  "H5P_LST_DATATYPE_ACCESS_ID_g")  hid-t)
(defcvar (#.(lispify "H5P_ATTRIBUTE_CREATE_DEFAULT" 'constant) "H5P_LST_ATTRIBUTE_CREATE_ID_g") hid-t)
(defcvar (#.(lispify "H5P_ATTRIBUTE_ACCESS_DEFAULT" 'constant) "H5P_LST_ATTRIBUTE_ACCESS_ID_g") hid-t)
(defcvar (#.(lispify "H5P_OBJECT_COPY_DEFAULT" 'constant)      "H5P_LST_OBJECT_COPY_ID_g")      hid-t)
(defcvar (#.(lispify "H5P_LINK_CREATE_DEFAULT" 'constant)      "H5P_LST_LINK_CREATE_ID_g")      hid-t)
(defcvar (#.(lispify "H5P_LINK_ACCESS_DEFAULT" 'constant)      "H5P_LST_LINK_ACCESS_ID_g")      hid-t)

