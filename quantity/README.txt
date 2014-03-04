The philosophy of use as I have it now is as follows:

Units are a subset of quantities, as they are just quantities which
have unit scale along a single dimension measured with the unit.  As
such, one can use generic math functions on units and/or quantities
transparently.

I represent units as keyword symbols, usually the lispified full names
of the unit as abbreviated names make for either clashing or difficult
to remember cryptic forms.  Auto-completion aids with this.

The S.I. system is used as the underlying implementation, so you
should define all units on top of the S.I. system ultimately.  This
does not need to be inconvenient however; once you have defined other
systems of units you can refer to them directly and they will be
automatically converted into the appropriate S.I. form.  This could
potentially cause a loss of precision, but in practice I think this is
not an issue.

Converting between units is done in two ways, the standard method and
the temperature method.  In the standard method, you simply divide
your quantity by the unit you wish to measure it against, and the
result will be the scale (the number of units the quantity evaluates
to).  This is manual, but it is the best way to proceed in my own
experience as it teaches the user what's really going on behind unit
conversion and also is the simplest and most compressed way to express
unit conversion I have found.  The temperature conversion method is
accomplished via linear functions; I have defined a library for
dealing with linear transformations/functions from one quantity to
another and have used this for implementing the temperature conversion
function convert-temperature.

Temperatures are the only place where there is a slight inconsistency
in the way unit conversions are handled, both for the methods
explained above and for the fact that you cannot rely on the
equivalence of quantities created via the generic math operations and
the reader macro/direct make-instance function.  The reason is that,
while other quantities are simply scaled units, temperatures are
scaled and shifted.  In general you should specify temperatures via
the reader macro/make-instance functions any time you will need to
refer to 0 on a temperature scale.  I may be able to repair this at
some point, but for now this is how it is.

Keyword symbols can be used as units without any extra definitions;
however there will be no defined conversion from that unit into the
S.I. system which is necessary for ease of conversion.
