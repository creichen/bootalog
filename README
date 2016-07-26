Bootalog: A simple Datalog engine (written in OCaml)
----------------------------------------------------

This is a toy implementation of Datalog with negation and infinite
domains that uses semi-naive evaluation.  While it hasn't otherwise
been optimised for storage or processing efficiency, you may find it
useful as a device for understanding Datalog evaluation or for writing
Datalog code.

If you want performance, try one of those instead:
- https://github.com/oracle/souffle
- http://www.logicblox.com

Status
------
Usable for experimentation, but the UI doesn't catch all usage errors
yet.

How to build
------------
make

Build Dependencies
------------------
- OCaml
  - str
  - num
  - oUnit

How to run
----------
./src/bootalog

How to use
----------
./src/bootalog --help
./src/bootalog --version
./src/bootalog [--data <data-file>] [--rules <rules-file>]

data-file follows the format described in `Bootalog data', below.
rules-file follows the format described in `Bootalog code', below.

Once started, run

  \help

to get help, or

  \?

to evaluate everything and print out all tables.  More operations are
available.

Datalog and Bootalog basics
---------------------------
Datalog relates ATOMS to each other via FACTS.  

An ATOM can be a string "foo", an integer number 42, or name 'my-name.

A FACT is a PREDICATE that relates one or more atoms:

  is-less-than(1, 2)

  is-german-for("two", "zwei")

  ie-even(2)

  is-owned-by('user, 'master-control-program)

Preicates are written in lower-case latters, connected with dashes.
Predicates correspond to tables in relational databases (which is
where Datalog originated).  For convenience, it is possible to give
names to the individual atoms as if they were fields in a record:

  book(name:"The Hitch Hiker's Guide to the Galaxy", author:"Douglas Adams", pub-year:1979)

Such labels are optional and can be mixed with non-labels:

  table(1, 2, sum:3)

The above is mildly useful for storing information, but it doesn't
help us access or process the underlying data.  For that, we instead
use Datalog rules.  Datalog rules have the form

  p0(VARS) :- p1(VARS1), p2(VARS2) ..., pn(VARSn) .

This rule computes all the valid variable bindings for VARS that are
satisfied by variables in p1(VARS1) ... pn(VARSn) (the rule's body)
and stores the result in predicate p0, as specified by p0(VARS) (the
rule's head).

  For instance, if we have the fact `is-german-for("two", "zwei")', we
can write

  all-my-german-words(X) :- is-german-for(Y, X).

and will get the fact `all-my-german-words("zwei")'.  Since we are not
using the variable Y here, we can replace it with a wildcard:

  all-my-german-words(X) :- is-german-for(_, X).

  Each wildcard (`_') is treated as if it were a fresh variable.


  Note that to actually teach the system that `is-german-for("two",
"zwei")' is a fact, you can use a rule with an empty body:

  is-german-for("two", "zwei") :- .

There is a short form for this:

  is-german-for("two", "zwei").


  Bootalog includes some built-in operations that operate over
`infinite domains' (integers and strings, specifically).  That means
that you can't print their tables-- neither directly or indirectly--
but you can use these on parameters whose values you obtain from
elsewhere (i.e., the parameters are atoms or obtained from tables).
For instance, let's say that you tell the system a couple of facts:

  n(1).
  n(3).
  n(5).

You can now use sys-add to add together those numbers:

  p(X) :- n(Y), n(Z), sys-add(Y, Z, sum:X).

  If you evaluate this rule, you will find that you now have the
following facts:

  p(2). p(4). p(6). p(8). p(10).

  Observe that the Datalog evaluator tries out all possible
combinations.  This even works with the built-in string concatenation function
str-concat(X, Y, concat:Z), which concatenates X, Y, and Z:

  q(X) :- sys-concat(X, _, concat:"foobar").

This will generate the following facts:

  q("f"). q("fo"). q("foo"). q("foob"). q("fooba"). q("foobar").

You can filter these facts by adding more constraints to the rule:

  r(X) :- sys-concat(X, _, concat:"foobar"), sys-length(X, length:L), sys-le(L, 3).

Here, `sys-length(X, length:L)' computes the length of string X, and
`sys-le(L, 3)' ensures that that length is <= 3.  Thus, this will only produce:

  r("f"). r("fo"). r("foo").

You can also negate constraints, by placing a `~' in front:

  r(X) :- sys-concat(X, _, concat:"foobar"), sys-length(X, length:L), ~sys-le(L, 3).

which will filter out all strings whose length is NOT less than or
equal to 3.


  There are other topics related to Datalog-- recursion, and multiple
rules with the same rule head (which cooperate `intelligently')-- that
you can learn more about by having a look at the literature.


Bootalog data
-------------
The optional data file contains literals made up of a predicate and
arbitary atoms, such as:

parent('c, 'cplusplus)
is-german-for("two", "zwei")

See `examples/parents.bld' for an example.


Bootalog code
-------------
This file contains arbitrary Datalog rules in the Bootalog format
described above.  In addition to rules of the form


  ancestor(X, Y) :- parent(X, Y).
  ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).

  indirect-ancestor(X, Y) :- ancestor(X, Y), ~parent(X, Y).

this can also include facts.


Built-ins
---------
- sys-concat(A, B, :CONCAT)
  Are the strings A and B, concatenated in that order, equal to CONCAT?

- sys-length(A, :LENGTH)
  Is the length of string A equal to LENGTH?

- sys-add(A, B, :SUM)
  Is A + B = SUM (ints only)?

- sys-sub(A, B, :DIFFERENCE)
  Is A - B = DIFFERENCE (ints only)?

- sys-mul(A, B, PRODUCT)
  Is A * B = PRODUCT (ints only)?

- sys-div(A, B, :QUOITIENT)
  Is A / B = QUOTIENT (ints only, quotient rounded down)?

- sys-modulo(A, B, :REMAINDER)
  Is A mod B = REMAINDER (ints only)?

- sys-lt(A, B)
  Is A < B?

- sys-le(A, B)
  Is A <= B?

- sys-gt(A, B)
  Is A > B?
  
- sys-ge(A, B)
  Is A >= B?

- =(A, B)
  Is A = B?


Author
------
Christoph Reichenbach <creichen@gmail.com