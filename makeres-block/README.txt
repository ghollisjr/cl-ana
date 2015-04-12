makeres-block provides the ability to define multiple targets within
one form.  Sometimes this is more convenient than individual target
definition.

The operator defresblock takes the list of ids defined as the first
argument and a body of expressions as the rest of the arguments.  The
only limitation to keep in mind at present is that setres or setresfn
should be used somewhere in the body to set the values of the defined
result targets, and nowhere in the body should (res id) be used for
one of the defined ids.  This may be fixed in the future, but at the
moment is not handled.
