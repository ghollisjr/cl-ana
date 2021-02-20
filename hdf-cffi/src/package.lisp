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

(defpackage #:hdf5
  (:documentation "hdf5-cffi library: Common LISP binding for the HDF5 library")
  (:use #:cl #:cffi)
  (:shadow "OFF-T" "SIZE-T")
  (:export #:load-hdf5-foreign-libraries
           #:+NULL+
           #:size-t
           #:time-t
           #:off-t

           ;; == h5 ===========================================================

           #:+H5-VERS-MAJOR+
           #:+H5-VERS-MINOR+
           #:+H5-VERS-RELEASE+

           #:herr-t
           #:hbool-t
           #:htri-t
           #:ssize-t
           #:hsize-t
           #:hssize-t
           #:haddr-t

           #:+HADDR-UNDEF+
           #:+HADDR-MAX+

           #:h5-iter-order-t

           #:+H5-ITER-ERROR+
           #:+H5-ITER-CONT+
           #:+H5-ITER-STOP+

           #:h5-index-t
           #:h5-ih-info-t

           #:h5allocate-memory
           #:h5check-version
           #:h5close
           #:h5dont-atexit
           #:h5free-memory
           #:h5garbage-collect
           #:h5get-libversion
           #:h5is-library-threadsafe
           #:h5open
           #:h5resize-memory
           #:h5set-free-list-limits

           ;; == h5i ==========================================================

           #:h5i-type-t

           #:hid-t

           #:+H5-SIZEOF-HID-T+
           #:+H5I-INVALID-HID+

           #:h5iclear-type
           #:h5idec-ref
           #:h5idec-type-ref
           #:h5idestroy-type
           #:h5iget-file-id
           #:h5iget-name
           #:h5iget-ref
           #:h5iget-type
           #:h5iget-type-ref
           #:h5iinc-ref
           #:h5inc-type-ref
           #:h5iis-valid
           #:h5inmembers
           #:h5iobject-verify
           #:h5iregister
           #:h5iregister-type
           #:h5iremove-verify
           #:h5isearch
           #:h5itype-exists

           ;; == h5f ==========================================================

           #:+H5F-ACC-RDONLY+
           #:+H5F-ACC-RDWR+
           #:+H5F-ACC-TRUNC+
           #:+H5F-ACC-EXCL+
           #:+H5F-ACC-DEBUG+
           #:+H5F-ACC-CREAT+
           #:+H5F-ACC-DEFAULT+

           #:+H5F-OBJ-FILE+
           #:+H5F-OBJ-DATASET+
           #:+H5F-OBJ-GROUP+
           #:+H5F-OBJ-DATATYPE+
           #:+H5F-OBJ-ATTR+
           #:+H5F-OBJ-ALL+
           #:+H5F-OBJ-LOCAL+

           #:h5f-scope-t

           #:+H5F-UNLIMITED+

           #:h5f-close-degree-t
           #:h5f-info-t
           #:h5f-libver-t

           #:h5fclear-elink-file-cache
           #:h5fclose
           #:h5fcreate
           #:h5fflush
           #:h5fget-access-plist
           #:h5fget-create-plist
           #:h5fget-file-image
           #:h5fget-filesize
           #:h5fget-freespace
           #:h5fget-info
           #:h5fget-intent
           #:h5fget-name
           #:h5fget-obj-count
           #:h5fget-obj-ids
           #:h5fis-hdf5
           #:h5fmount
           #:h5fopen
           #:h5freopen
           #:h5funmount

           ;; == h5t ==========================================================

           #:h5t-class-t
           #:h5t-order-t
           #:h5t-sign-t
           #:h5t-norm-t
           #:h5t-cset-t

           #:+H5T-NCSET+

           #:h5t-str-t

           #:+H5T-NSTR+

           #:h5t-pad-t
           #:h5t-cmd-t
           #:h5t-bkg-t
           #:h5t-cdata-t
           #:h5t-pers-t
           #:h5t-dir-t
           #:h5t-conv-except-t
           #:h5t-conv-ret-t
           #:hvl-t

           #:+H5T-VARIABLE+

           #:+H5T-OPAQUE-TAG-MAX+

           #:+H5T-IEEE-F32BE+
           #:+H5T-IEEE-F32LE+
           #:+H5T-IEEE-F64BE+
           #:+H5T-IEEE-F64LE+
           #:+H5T-STD-I8BE+
           #:+H5T-STD-I8LE+
           #:+H5T-STD-I16BE+
           #:+H5T-STD-I16LE+
           #:+H5T-STD-I32BE+
           #:+H5T-STD-I32LE+
           #:+H5T-STD-I64BE+
           #:+H5T-STD-I64LE+
           #:+H5T-STD-U8BE+
           #:+H5T-STD-U8LE+
           #:+H5T-STD-U16BE+
           #:+H5T-STD-U16LE+
           #:+H5T-STD-U32BE+
           #:+H5T-STD-U32LE+
           #:+H5T-STD-U64BE+
           #:+H5T-STD-U64LE+
           #:+H5T-STD-B8BE+
           #:+H5T-STD-B8LE+
           #:+H5T-STD-B16BE+
           #:+H5T-STD-B16LE+
           #:+H5T-STD-B32BE+
           #:+H5T-STD-B32LE+
           #:+H5T-STD-B64BE+
           #:+H5T-STD-B64LE+
           #:+H5T-STD-REF-OBJ+
           #:+H5T-STD-REF-DSETREG+

           #:+H5T-UNIX-D32BE+
           #:+H5T-UNIX-D32LE+
           #:+H5T-UNIX-D64BE+
           #:+H5T-UNIX-D64LE+

           #:+H5T-C-S1+
           #:+H5T-FORTRAN-S1+

           #:+H5T-NATIVE-CHAR+
           #:+H5T-NATIVE-SCHAR+
           #:+H5T-NATIVE-UCHAR+
           #:+H5T-NATIVE-SHORT+
           #:+H5T-NATIVE-USHORT+
           #:+H5T-NATIVE-INT+
           #:+H5T-NATIVE-UINT+
           #:+H5T-NATIVE-LONG+
           #:+H5T-NATIVE-ULONG+
           #:+H5T-NATIVE-LLONG+
           #:+H5T-NATIVE-ULLONG+
           #:+H5T-NATIVE-FLOAT+
           #:+H5T-NATIVE-DOUBLE+
           #:+H5T-NATIVE-B8+
           #:+H5T-NATIVE-B16+
           #:+H5T-NATIVE-B32+
           #:+H5T-NATIVE-B64+
           #:+H5T-NATIVE-OPAQUE+
           #:+H5T-NATIVE-HADDR+
           #:+H5T-NATIVE-HSIZE+
           #:+H5T-NATIVE-HSSIZE+
           #:+H5T-NATIVE-HERR+
           #:+H5T-NATIVE-HBOOL+

           #:h5tarray-create2
           #:h5tclose
           #:h5tcommit2
           #:h5tcommit-anon
           #:h5tcommitted
           #:h5tcompiler-conv
           #:h5tconvert
           #:h5tcopy
           #:h5tcreate
           #:h5tdecode
           #:h5tdetect-class
           #:h5tencode
           #:h5tenum-create
           #:h5tenum-insert
           #:h5tenum-nameof
           #:h5tenum-valueof
           #:h5tequal
           #:h5tfind
           #:h5tget-array-dims2
           #:h5tget-array-ndims
           #:h5tget-class
           #:h5tget-create-plist
           #:h5tget-cset
           #:h5tget-ebias
           #:h5tget-fields
           #:h5tget-inpad
           #:h5tget-member-class
           #:h5tget-member-index
           #:h5tget-member-name
           #:h5tget-member-offset
           #:h5tget-member-type
           #:h5tget-member-value
           #:h5tget-native-type
           #:h5tget-nmembers
           #:h5tget-norm
           #:h5tget-offset
           #:h5tget-order
           #:h5tget-pad
           #:h5tget-precision
           #:h5tget-sign
           #:h5tget-size
           #:h5tget-strpad
           #:h5tget-super
           #:h5tget-tag
           #:h5tinsert
           #:h5tis-variable-string
           #:h5tlock
           #:h5topen2
           #:h5tpack
           #:h5tregister
           #:h5tset-cset
           #:h5tset-ebias
           #:h5tset-fields
           #:h5tset-inpad
           #:h5tset-norm
           #:h5tset-offset
           #:h5tset-order
           #:h5tset-pad
           #:h5tset-precision
           #:h5tset-sign
           #:h5tset-size
           #:h5tset-strpad
           #:h5tset-tag
           #:h5tunregister
           #:h5tvlen-create

           ;; == h5l ==========================================================

           #:+H5L-MAX-LINK-NAME-LEN+
           #:+H5L-SAME-LOC+
           #:+H5L-LINK-CLASS-T-VERS+

           #:h5l-type-t

           #:+H5L-TYPE-BUILTIN-MAX+
           #:+H5L-TYPE-UD-MIN+

           #:h5l-info-t

           #:h5lcopy
           #:h5lcreate-external
           #:h5lcreate-hard
           #:h5lcreate-soft
           #:h5lcreate-ud
           #:h5ldelete
           #:h5ldelete_by-idx
           #:h5lexists
           #:h5lget-info
           #:h5lget-info-by-idx
           #:h5lget-name-by-idx
           #:h5lget-val
           #:h5lget-val-by-index
           #:h5lis-registered
           #:h5literate
           #:h5literate-by-name
           #:h5lmove
           #:h5lregister
           #:h5lunpack-elink-val
           #:h5lunregister
           #:h5lvisit
           #:h5lvisit-by-name

           ;; == h5o ==========================================================

           #:+H5O-COPY-SHALLOW-HIERARCHY-FLAG+
           #:+H5O-COPY-EXPAND-SOFT-LINK-FLAG+
           #:+H5O-COPY-EXPAND-EXT-LINK-FLAG+
           #:+H5O-COPY-EXPAND-REFERENCE-FLAG+
           #:+H5O-COPY-WITHOUT-ATTR-FLAG+
           #:+H5O-COPY-PRESERVE-NULL-FLAG+
           #:+H5O-COPY-MERGE-COMMITTED-DTYPE-FLAG+
           #:+H5O-COPY-ALL+

           #:+H5O-SHMESG-NONE-FLAG+
           #:+H5O-SHMESG-SDSPACE-FLAG+
           #:+H5O-SHMESG-DTYPE-FLAG+
           #:+H5O-SHMESG-FILL-FLAG+
           #:+H5O-SHMESG-PLINE-FLAG+
           #:+H5O-SHMESG-ATTR-FLAG+
           #:+H5O-SHMESG-ALL-FLAG+
           #:+H5O-SHMESG-MAX-NINDEXES+
           #:+H5O-SHMESG-MAX-LIST-SIZE+

           #:+H5O-HDR-CHUNK0-SIZE+
           #:+H5O-HDR-ATTR-CRT-ORDER_TRACKED+
           #:+H5O-HDR-ATTR-CRT-ORDER_INDEXED+
           #:+H5O-HDR-ATTR-STORE-PHASE-CHANGE+
           #:+H5O-HDR-STORE-TIMES+
           #:+H5O-HDR-ALL-FLAGS+

           #:h5o-type-t
           #:h5o-msg-crt-idx-t
           #:h5o-hdr-info-t
           #:h5o-info-t

           #:h5oclose
           #:h5ocopy
           #:h5odecr-refcount
           #:h5oexists-by-name
           #:h5oget-comment
           #:h5oget-comment-by-name
           #:h5oget-info
           #:h5oget-info-by-idx
           #:h5oget-info-by-name
           #:h5oincr-refcount
           #:h5olink
           #:h5oopen
           #:h5oopen_by_addr
           #:h5oopen_by_idx
           #:h5ovisit
           #:h5ovisit-by-name

           ;; ==#:h5s ==========================================================

           #:+H5S-ALL+
           #:+H5S-MAX-RANK+
           #:+H5S-UNLIMITED+

           #:h5s-class-t
           #:h5s-sel-type
           #:h5s-seloper-t

           #:h5sclose
           #:h5scopy
           #:h5screate
           #:h5screate-simple
           #:h5sdecode
           #:h5sencode
           #:h5sextent-copy
           #:h5sextent-equal
           #:h5sget-select-bounds
           #:h5sget-select-elem-npoints
           #:h5sget-select-elem-pointlist
           #:h5sget-select-hyper-blocklist
           #:h5sget-select-hyper-nblocks
           #:h5sget-select-npoints
           #:h5sget-select-type
           #:h5sget-simple-extent-dims
           #:h5sget-simple-extent-ndims
           #:h5sget-simple-extent-npoints
           #:h5sget-simple-extent-type
           #:h5sis-simple
           #:h5soffest-simple
           #:h5sselect-all
           #:h5sselect-elements
           #:h5sselect-hyperslab
           #:h5sselect-none
           #:h5sselect-valid
           #:h5sset-extent-none
           #:h5sset-extent-simple

           ;; ==#:h5d ==========================================================

           #:h5d-alloc-time-t
           #:h5d-fill-time-t
           #:h5d-fill-value-t
           #:h5d-layout-t
           #:h5d-space-status-t

           #:h5dclose
           #:h5dcreate1
           #:h5dcreate2
           #:h5dcreate-anon
           #:h5dfill
           #:h5dgather
           #:h5dget-access-plist
           #:h5dget-create-plist
           #:h5dget-offset
           #:h5dget-space
           #:h5dget-space-status
           #:h5dget-storage-size
           #:h5dget-type
           #:h5diterate
           #:h5dopen2
           #:h5dread
           #:h5dscatter
           #:h5dset-extent
           #:h5dvlen-get-buf-size
           #:h5dvlen-reclaim
           #:h5dwrite

           ;; ==#:h5g ==========================================================

           #:h5g-storage-type-t
           #:h5g-info-type-t

           #:h5gclose
           #:h5gcreate1
           #:h5gcreate2
           #:h5gcreate-anon
           #:h5gcreate-plist
           #:h5gget-info
           #:h5gget-info-by-idx
           #:h5gget-info-by-name
           #:h5gopen1
           #:h5gopen2

           ;; ==#:h5a ==========================================================

           #:h5a-info-t

           #:h5aclose
           #:h5acreate1
           #:h5acreate2
           #:h5acreate-by-name
           #:h5adelete
           #:h5adelete-by-idx
           #:h5adelete-by-name
           #:h5aexists
           #:h5aexists-by-name
           #:h5aget-create-plist
           #:h5aget-info
           #:h5aget-info-by-idx
           #:h5aget-info-by-name
           #:h5aget-name
           #:h5aget-name-by-idx
           #:h5aget-space
           #:h5aget-storage-size
           #:h5aget-type
           #:h5aiterate2
           #:h5aiterate-by-name
           #:h5aopen
           #:h5aopen-by-idx
           #:h5aopen-by-name
           #:h5aread
           #:h5arename
           #:h5arename-by-name
           #:h5awrite

           ;; ==#:h5r ==========================================================

           #:h5r-type-t

           #:+H5R-OBJ-REF-BUF-SIZE+

           #:hobj-ref-t

           #:+H5R-DSET-REG-REF-BUF-SIZE+

           #:hdset-reg-ref-t

           #:h5rcreate
           #:h5rdereference
           #:h5rget-name
           #:h5rget-obj-type2
           #:h5rget-region

           ;; ==#:h5z ==========================================================

           #:h5z-edc-t
           #:h5z-filter-t
           #:h5z-so-scale-type-t

           #:+H5Z-FILTER-ERROR+
           #:+H5Z-FILTER-NONE+
           #:+H5Z-FILTER-DEFLAT+
           #:+H5Z-FILTER-SHUFFLE+
           #:+H5Z-FILTER-FLETCHER32+
           #:+H5Z-FILTER-SZIP+
           #:+H5Z-FILTER-NBIT+
           #:+H5Z-FILTER-SCALEOFFSET+
           #:+H5Z-FILTER-RESERVED+
           #:+H5Z-FILTER-MAX+
           #:+H5Z-FILTER-ALL+
           #:+H5Z-MAX-NFILTERS+
           #:+H5-SZIP-ALLOW-K13-OPTION-MASK+
           #:+H5-SZIP-CHIP-OPTION-MASK+
           #:+H5-SZIP-EC-OPTION-MASK+
           #:+H5-SZIP-NN-OPTION-MASK+
           #:+H5-SZIP-MAX-PIXELS-PER-BLOCK+
           #:+H5Z-SHUFFLE-USER-NPARMS+
           #:+H5Z-SHUFFLE-TOTAL-NPARMS+
           #:+H5Z-SZIP-USER-NPARMS+
           #:+H5Z-SZIP-TOTAL-NPARMS+
           #:+H5Z-SZIP-PARM-MASK+
           #:+H5Z-SZIP-PARM-PPB+
           #:+H5Z-SZIP-PARM-BPP+
           #:+H5Z-SZIP-PARM-PPS+
           #:+H5Z-NBIT-USER-NPARMS+
           #:+H5Z-SCALEOFFSET-USER-NPARMS+
           #:+H5Z-SO-INT-MINBITS-DEFAULT+
           #:+H5Z-CLASS-T-VERS+
           #:+H5Z-FILTER-CONFIG-ENCODE-ENABLED+
           #:+H5Z-FILTER-CONFIG-DECODE-ENABLED+

           #:h5zfilter-avail
           #:h5zget-filter-info

           ;; ==#:h5p ==========================================================

           #:+H5P-DEFAULT+

           #:+H5P-ROOT+
           #:+H5P-OBJECT-CREATE+
           #:+H5P-FILE-CREATE+
           #:+H5P-FILE-ACCESS+
           #:+H5P-DATASET-CREATE+
           #:+H5P-DATASET-ACCESS+
           #:+H5P-DATASET-XFER+
           #:+H5P-FILE-MOUNT+
           #:+H5P-GROUP-CREATE+
           #:+H5P-GROUP-ACCESS+
           #:+H5P-DATATYPE-CREATE+
           #:+H5P-DATATYPE-ACCESS+
           #:+H5P-STRING-CREATE+
           #:+H5P-ATTRIBUTE-CREATE+
           #:+H5P-OBJECT-COPY+
           #:+H5P-LINK-CREATE+
           #:+H5P-LINK-ACCESS+

           #:+H5P-FILE-CREATE-DEFAULT+
           #:+H5P-FILE-ACCESS-DEFAULT+
           #:+H5P-DATASET-CREATE-DEFAULT+
           #:+H5P-DATASET-ACCESS-DEFAULT+
           #:+H5P-DATASET-XFER-DEFAULT+
           #:+H5P-FILE-MOUNT-DEFAULT+
           #:+H5P-GROUP-CREATE-DEFAULT+
           #:+H5P-GROUP-ACCESS-DEFAULT+
           #:+H5P-DATATYPE-CREATE-DEFAULT+
           #:+H5P-DATATYPE-ACCESS-DEFAULT+
           #:+H5P-ATTRIBUTE-CREATE-DEFAULT+
           #:+H5P-OBJECT-COPY-DEFAULT+
           #:+H5P-LINK-CREATE-DEFAULT+
           #:+H5P-LINK-ACCESS-DEFAULT+

           #:h5pclose
           #:h5pcreate
           #:h5pcopy
           #:h5pget-char-encoding
           #:h5pget-chunk
           #:h5pget-class
           #:h5pget-core-write-tracking
           #:h5pget-create-intermediate-group
           #:h5pget-external
           #:h5pget-external-count
           #:h5pget-fapl-core
           #:h5pget-fclose-degree
           #:h5pget-file-image
           #:h5pget-fill-value
           #:h5pget-filter2
           #:h5pget-layout
           #:h5pget-libver-bounds
           #:h5pget-nfilters
           #:h5pget-sizes
           #:h5pget-userblock
           #:h5pget-version
           #:h5pset-alloc-time
           #:h5pset-char-encoding
           #:h5pset-chunk
           #:h5pset-core-write-tracking
           #:h5pset-create-intermediate-group
           #:h5pset-data-transform
           #:h5pset-deflate
           #:h5pset-external
           #:h5pset-fapl-core
           #:h5pset-fclose-degree
           #:h5pset-file-image
           #:h5pset-fill-value
           #:h5pset-fletcher32
           #:h5pset-layout
           #:h5pset-link-creation-order
           #:h5pset-link-phase-change
           #:h5pset-libver-bounds
           #:h5pset-nbit
           #:h5pset-scaleoffset
           #:h5pset-shuffle
           #:h5pset-szip
           #:h5pset-userblock

           ;; ==#:h5pl =========================================================

           #:h5plget-loading-state
           #:h5plset-loading-state))
