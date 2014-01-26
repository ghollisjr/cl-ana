This project contains multiple libraries which are grouped together
either due to mutual dependencies or my unwillingness to maintain
dozens of individual projects while I'm developing my physics analysis
software.

As you will see in reading the code, I've tried to keep everything
well documented.  I hope that development of this code keeps to strict
standards of documentation so that it will be easy to understand.

The basic development strategy I've attempted to use is bottom-up.
Unless there is a proven/mathematical structure in place, I try to not
limit possibilities for use; this leads in some cases to extra
functions which may never get used.  I've tried to create useful
layers of functionality so that at each step I can do bigger things
with what I've just written.  I hope that this approach really will
pay off as some (such as the Apostle Paul Graham) have claimed.

So far the features I've implemented are:

--------------------------------------------------
1. Generic mathematics

Common lisp comes out of the box lacking generic mathematics; i.e. one
cannot simply extend the +, -, /, *, sqrt, etc. functions.  I looked
for libraries which already did this, and found among them Antik.
However: Antik's structure was heavily biased (IMO) towards grids &
arrays.  So, I decided to just build the basic mathematical generic
functions which can sit atop the built-in CL functions as well as
anything that libraries like Antik provide.

This resulted in the development of a simple error propogation library
among other benefits for histogramming, data fitting, linear algebra
(no I did not re-implement this), etc.

--------------------------------------------------
2. Error propogation

The error-propogation library provides err-num numerical objects which
automatically perform error propogation during a computation.  This
was done via specializing on the generic math functions as provided by
the generic math library.

It also provides the reader macro #e() which allows one to write

#e(x y ...)

to mean

x +- y +- ....

The structure is recursive so that each value (other than the first)
is the error in the preceding value.  The generic math library is
implemented in such a way that the propogation is done correctly at
all levels of error (precluding bugs that is).

As a similar physics-based numerical concept, I would like to at some
point implement physical quantities (values with dimensions like
length, mass, etc.) as well; I think Antik does this as well so it
might be possible to simply interface with it; we'll see.  Evan
mentioned something about Dr. Johnson's (I think?) work on efficient
representation of quantities with errors, so I can try to investigate
this.

--------------------------------------------------
3. Tables

Tables are 2 dimensional data storage abstractions (think of the awk
Unix utility).  A table is composed of a number of rows.  Each row is
composed of a number of fields.  Each row must have the same number
and kinds of fields; i.e. tables are rectangular.

Tables provide at a minimum sequential reading from/writing into, but
in general a table when opened for reading is only available for
reading, and the same goes for tables opened for writing.

(I intend to provide higher level table management functionality that
abstracts away this limitation by automatically managing the physical
table objects behind the scenes.)

For writing, one can modify the fields of a row and then commit the
row into the table.

For reading, one can use the dotable macro which creates a loop body
inside which the field values for each row are already known; all you
have to do is reference a (marked) field variable name and it will be
read into memory each iteration of the loop.

So far I've implemented the following table types:

hdf-table: Provides access to HDF5 datasets.  The access is limited to
1-dimensional datasets with compound datatypes as the dataset datum
type.  Allows random read access.  Related is the read-only
hdf-table-chain object which chains together a list of files
containing the same type of HDF5 dataset as a single table, managing
files and reading data into memory as needed.

csv-table: Provides access to CSV (Comma Separated Values) files.
Convenient for interfacing with spreadsheet data.  Can be read or
written.

ntuple-table: Provides access to ntuple datasets as implemented by GSL
(uses GSLL).  Can be read or written, though the ntuple format does
not include any information on the type of data it stores, so you have
to keep track of it yourself.

plist-table: Creates a readable table object from a list of property
lists (known as plists in LISP terminology) where each plist is
interpreted to be a row.  Handy for small-scale in-memory computation;
I used it for debugging mainly.

table-chain: Chains together tables of any type for reading, provided
they have the same row structure.  Could be handy for implementing the
higher-level table management functionality.

--------------------------------------------------
4. Histograms

Histograms are currently provided in two types: Contiguous and Sparse.

Contiguous histograms store histogram bin values in-memory in a
recursive array of arrays of arrays ....  This is inefficient in
memory when there are many empty bins, but is efficient in computing
time for arithmetic operations, integration, etc.

Sparse histograms are implemented using hash tables so that empty bin
values are not stored.  This is efficient in memory when the data is
sparse but inefficient for arithmetic operations--so much so that at
the moment I neglect to implement these functions.

In order to remedy the limitations with these different types of
histograms, I've thought of implementing a histogram computation type
which builds up a computation to do using multiple histograms as input
(of either type) where the resulting histogram will be integrated down
to a manageable number of dimensions at the end of the computation.
This will be done efficiently by commuting addition/subtraction with
the integration as far as can be done.  I'm not sure how useful this
will be however, and this is on the back burner for now.

--------------------------------------------------
5. Fitting

Specifically, nonlinear least squares fitting as implemented by GSL.

The GSL library is supposed to implement the fitting without need for
an analytic Jacobian, but using GSLL I can't seem to figure out how to
use it/there is a bug in the interface or implementation, since every
time I call the appropriate function it fails with a memory fault
while inside a foreign function call.

However, the analytical Jacobian version works fine, so I just
implemented a higher-order function which creates the Jacobian
function given an input function.  This seems to remedy the situation
fine; I haven't seen any performance or accuracy issues so far in the
testing.

Fitting is done using lisp functions instead of special fit-able
function objects.  Some things about the function are inferred from
the other arguments, for example the number of fit parameters is
inferred from the initial parameter guesses you provide the fit
routine.

To integrate your data container with the fitting routine, you have to
specialize on a method which generates an alist mapping the
independent data values to the dependent ones, but that's all that's
required.

--------------------------------------------------
6. Lorentz vectors/boosts

I've implemented a simple lorentz vector and boosting library on top
of lisp-matrix, which is available via quicklisp.

It's integrated into the generic math framework as well, so some of
the functions such as +, -, *, / will work.

Boosting is done by creating a boost matrix and then applying the
matrix to the lorentz vector; there are convenient functions written
to do this.  It seemed more efficient to create a single boost matrix
and then multiply many lorentz vectors instead of re-creating the
matrix every time you want to boost a single vector.

--------------------------------------------------
7. Plotting

Plotting is as of yet incomplete; it is possible to use what I have
completed to plot things, but you have to know gnuplot's commands to
do so.

My intentions are to mimick the structure of the gnuplot objects in
lisp, and in so doing provide a plotting library in lisp.

There are already a few dead/incomplete projects that attempted to do
this; apparently those poor souls who tried got stuck in limbo or
something.

I don't forsee that happening to me, as I've done something similar
already during my honors thesis where I would generate gnuplot
commands and data to pipe into said commands in order to draw Venn
diagrams.  I did that with shell scripting and a little C++, so with
LISP at my disposal it should be much easier.

--------------------------------------------------
Appendix: Miscellaneous libraries

In the process of creating the other functionality, I've had to create
several small utilities & libraries.

For example, there a number of x-utils libraries: list-utils,
string-utils, functional-utils, macro-utils, and lisp-matrix-utils.
These provide small functionality in the appropriate subdomains.

Of special note is the memoization library, which provides a new
function definition operator defun-memoized which defines a memoized
function.  These functions remember previous return values so that
lengthy computations/tree-like recursive patterns can be efficiently
computed.  It is implemented via hash tables.

Then there is the tensor library, which generalizes the idea of
mapping to sequences of arbitrary structural depth, e.g. lists of
lists, or arrays of lists of arrays of ....  It also provides several
useful functions which can be defined on top of the tensor-map
function, such as tensor-+, tensor--, and other arithmetic operations.
I used this to implement some of the contiguous histogram functions.
It could in principle be used to do matrix operations as well, or
(obviously) tensor operations.

The typespec library (and hdf-typespec) provides typed data
descriptions that can be used with CFFI.  typespec provides a function
to generate a CFFI cstruct from the typespec.  hdf-typespec provides
the ability to convert between HDF5 data types and CFFI types via
typespecs.  This allows the generation of cstruct types after reading
the HDF5 data type from an HDF file.  I honestly don't know how I
would have done this without using code-writing software in C++, or
pretty much any other statically typed language for that matter.

Lastly there are the foreign function interfaces I've written to
either provide interfaces with foreign libraries or extend/repair the
ones provided by other libraries I depend on, such as GSLL.
