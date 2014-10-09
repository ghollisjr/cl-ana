makeres-table is a collection of graph transformations for use with
makeres for efficiently analyzing large structured datasets.

makeres-table is free (GPL) software, and is built on top of makeres,
makeres-macro, and cl-ana (all GPL).

The utilities provided by the various operators are:

* Merging passes over tables which could be done in parallel into a
  single pass.

* Distinction between physical (written and read via disk/memory) and
  logical (always computed from physical sources) tables.

--------------------------------------------------
The operators provided are:

* dotab: General-purpose table iteration; used for non-table results.

* tab: Iteration over a table to produce a physical table.

If the source table for a physical table needs to be iterated, then as
many of the necessary passes over the physical table are generated
during the source table iteration (known as collapsing a pass).  The
assumption is that disk access is the bottleneck, so that as much
computation should be done per disk accesss as possible.

* ltab: Iteration over a table to produce a logical table.

Tables created via ltab are vaporous; they are not part of the final
computation (technically they always have the value nil), and any
results to be generated from an ltab are computed from an appropriate
physical source via pass collapsing.  This is done prior to physical
table pass collapsing, so the actual source table passed over by a
reduction of a logical table depends on the physical tables.

The primary use of logical tables is to denote subsets of data which
do not really deserve to be written to disk, but are still helpful for
describing a computation (e.g. applying filters or defining a new
structure for a subset of data).

Any fields from the source table are available as long as they are not
shadowed by stating them in the call to push-fields in the ltab
definition.

* deflfields: Definition of logical fields.

Logical fields are analogous to logical tables in that they are always
computed per row; they provide an alternative to physically storing
fields which could be computed based on already available information.
