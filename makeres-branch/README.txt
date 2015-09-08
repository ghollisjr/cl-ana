makeres-branch allows for easily defining computations which will be
evaluated for various settings of variables; each setting of the
variables is known as a branch of the computation.

As an example, suppose there is a dataset available which contains the
religiosity and IQ scores for various individuals available in a CSV
file.  We could define the source table via

    (defres src
      (srctab (csv-opener "path/to/file"
                          :read-from-string t)))

If we wished to calculate the mean IQ score of the total population,
we could write

    (defres (src mean-IQ)
      (dotab (res src)
          ((sum 0)
           (count 0))
          (/ sum count)
        (incf sum (field IQ))
        (incf count)))

Now suppose we wish to compare the mean IQ scores of individuals from
different levels of religiosity, say those below 50% and above 50%
religiosity (whatever that would mean).  Without branch
transformations, we might do the following:

    (defres (src religious mean-IQ)
        ((sum 0)
         (count 0))
        (/ sum count)
      (when (> (field religiosity)
               0.5)
        (incf sum (field IQ))
        (incf count)))

However, branching allows us to do the following:

    (defparameter *religiosity-ranges*
      '((list 0 0.5)
        (list 0.5 1)))

    (defres (src religious mean-IQ)
      (branch *religiosity-ranges*
        (dotab (res src)
            ((sum 0)
             (count 0))
            (/ sum count)
          (when (let ((lst (branch)))
                  (<= (first lst) (field religiosity) (second lst)))
            (incf sum (field IQ))
            (incf count)))))

The branch operator serves two purposes:

1. The outermost branch operator defines a branching computation.  The
first argument is known as the branch-list and should be either a form
which when evaluated by branctrans results in a list of forms which
will be used in the body of each branch of the computation, or a (res
id) form which denotes that it should branch in parallel to another
branching computation using the same branch values.

2. The branch operators present inside a branching computation are
substituted with the value of the branch form for that branch of the
computation.  The branch forms are thus supplied at compile time to
the branching computation.  If not supplied any arguments,
e.g. (branch), it evaluates to the inner-most branch in the case of
nested branches (explained below).  It also accepts a single argument
which should be the branch list supplied to one of the outermost
branch forms in order to select which branch value it refers to.

The result of a branching computation is a hash-table mapping from
each branch value (at run time) to the result of the computation
branch.

Branching can be nested, e.g.

    (defparameter *xs* (list 1 2 3))
    (defparameter *ys* (list ''a ''b ''c)) ; double quotes needed

    (defres double-branch
      (branch *xs*
        (branch *ys*
          (cons (branch *xs*) (branch *ys*)))))

would result in nested hash tables mapping to conses of every possible
pair from *xs* and *ys*.

What would be nice would be to support branch-lists which are
themselves result targets, but this would require rennovation of the
graph transformation pipeline.
