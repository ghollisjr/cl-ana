The usage model for the hdf-table is:

You can 1. Read an hdf-table from a file,
    	2. Create an hdf-table with an associated file.

Reading and writing are done with buffering; the buffer size during
the read process defaults to the chunk size of the dataset.

In the functions, open --> create a readable hdf-table,
                  make --> create a writeable hdf-table.

The associated hdf-table-chain is available for reading a chain of
files which contain datasets with the same path in the file and of the
same type.  This is sometimes useful when the data you have access to
is given in many files instead of a single file.

***** READ THIS *****

Whenever you are done with an hdf-table (or chain), you should always
call table-close on the table object.  This is necessary since I'm
storing the dataset object inside the hdf-table (and since CLOS
doesn't handle object destructors).

--------------------------------------------------

There are two kinds of hdf-table objects:

1. Type conversion included (hdf-table, hdf-table-chain)
2. Type conversion not included (raw-hdf-table, raw-hdf-table-chain)

Type conversion is expensive but nice.  For small datasets, type
conversion can save time writing the code, but for large datasets type
conversion can cost huge amounts of time.
