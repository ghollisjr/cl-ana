makeres-tabletrans is a collection of graph transformations for use with
makeres.

makeres-tabletrans is free (GPL) software.

The utilities provided by the various operators are:

* Merging passes over tables which could be done in parallel into a
  single pass.

* Distinction between physical (written and read via disk/memory) and
  logical (always computed from physical sources) tables.

* Operators filter and trans for returning filtered or structurally
  transformed tables (e.g. remove rows with field x less than 2,
  add/remove fields, etc.)
