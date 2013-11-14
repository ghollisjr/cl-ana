The table class is an abstract representing the basic idea of a table.

Tables can be written to/read from a field (column & row) at a time.

When writing to a table, you first set the field values for the
current row and then push the row onto the table object using
table-commit-row.

dotable is at the moment the best way to read data from a table
object, since it allows you to loop over the rows in the table,
automatically creating lexically scoped variables inside the loop
which store the values of the table fields for the row.

I am considering an even higher level construct which will allow the
assimilation of various data sources into a single table, creation of
new tables which are the result of filtering another table according
to some criteria, etc.  It would be done within a functional
framework, so that the various computations to be done resulting in
logical or physical columns will be turned into a loop over the
contents of the constituent tables, and the outputs can be specified
in a reduce/fold paradigm perhaps, not sure about that.  The beauty is
that the dotable macro will still apply as long as I correctly
implement the get/set field, commit row, and column-names functions.
