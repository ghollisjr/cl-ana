

The groveler code grovels some constant values such as `H5F_ACC_RDONLY` .
However, here is a problem: the definition of `H5F_ACC_RDONLY` is as follows:

H5Fpublic.h
``` C
#define H5F_ACC_RDONLY	(H5CHECK H5OPEN 0x0000u)	/*absence of rdwr => rd-only */
```

What are H5CHECK and H5OPEN ? These are calls to *functions*:

H5Fpublic.h
``` C
/* When this header is included from a private header, don't make calls to H5check() */
#undef H5CHECK
#ifndef _H5private_H
#define H5CHECK          H5check(),
#else   /* _H5private_H */
#define H5CHECK
#endif  /* _H5private_H */

/* When this header is included from a private HDF5 header, don't make calls to H5open() */
#undef H5OPEN
#ifndef _H5private_H
#define H5OPEN        H5open(),
#else   /* _H5private_H */
#define H5OPEN
#endif  /* _H5private_H */
```

As the source code comment says,

``` C
/*
 * These are the bits that can be passed to the `flags' argument of
 * H5Fcreate() and H5Fopen(). Use the bit-wise OR operator (|) to combine
 * them as needed.  As a side effect, they call H5check_version() to make sure
 * that the application is compiled with a version of the hdf5 header files
 * which are compatible with the library to which the application is linked.
 * We're assuming that these constants are used rather early in the hdf5
 * session.
 *
 * Note that H5F_ACC_DEBUG is deprecated (nonfuncational) but retained as a
 * symbol for backward compatibility.
 */
#define H5F_ACC_RDONLY	(H5CHECK H5OPEN 0x0000u)	/*absence of rdwr => rd-only */
#define H5F_ACC_RDWR	(H5CHECK H5OPEN 0x0001u)	/*open for read and write    */
...
```

Thus, constants such as `H5F_ACC_RDONLY` not only serves as constants but also
checks the compatibility between the shared lib and the header file.


When you try to grovel the value of `H5F_ACC_RDONLY` as you normally do,
the corresponding grovel C file expands into a code that contains `H5check()` and `H5open()`.
When this C code gets compiled,
the linker complains that the definitions of these functions are missing in the executable.
The grovelling fails.

Thus it is necessary to define the private flag `_H5private_H` in order to disable these function calls.
