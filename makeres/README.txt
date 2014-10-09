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
