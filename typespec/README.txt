The Typespec library is a set of functions designed to make it easy to
generate/convert between foreign types & structures.

For example, one might wish to generate both a C-structure with CFFI
and a corresponding HDF5 data type.  They will have the same basic
structure; this is what I refer to as a typespec.

A typespec is one of the following:

1. A CFFI type symbol, such as :int, :double, or a user-defined CFFI
type.  This directly represents the CFFI type.

2. A list where the first element is the keyword symbol :compound
consed onto the front of an alist where the cars are lisp strings
representing the names of the fields in the structure, and the cdrs
are the typespecs of each field.

3. A list where the first element is the keyword symbol :array
followed by the typespec of the elements, the rank of the array as an
integer, and a list of the sizes of the array in each dimension.

This covers the most basic types understood by C and other languages,
so it should be (correct this if wrong) enough for most purposes.

The library comes with functions for generating CFFI cstructs from
typespecs, and there is the hdf-typespec project for generating
typespecs from HDF5 types or vice versa.
