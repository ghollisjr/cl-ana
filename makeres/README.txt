makeres and logres used to be separate pieces, here are their readmes:

--------------------------------------------------

makeres is a free (GPL) Common Lisp make-like library for performing
computations.  What sets makeres apart is the ability to
insert/manipulate a graph transformation pipeline which is used to
provide meaning and/or optimizations to computations which otherwise
would be expensive as written.

makeres is built on top of cl-ana (only small need however, may sever
connection later)

test.lisp gives test cases which also serve as examples of how to use
makeres.

The Cells library was close to what I needed, so I feel bad about
making a whole new tool to do make-like computations in lisp, but
perhaps it will be useful to someone else as well.

The wiki (insert wiki url) provides more complete documentation and
usage tutorials.

--------------------------------------------------

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
