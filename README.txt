logres is a free (GPL) Common Lisp library/tool for logging results
computed with makeres (loading and storing).

Currently due to implementation-specific directory functionality and
cl-fad not working as it stands with SBCL, the only implementation
supported is SBCL.

logres provides a framework inside which one can build more complete
logging functionality.  For example: logres is not well-suited for
logging file-based results since the information exists on disk
specifically and not completely in memory, so loading/storing
file-based resources is better handled by other tools.
