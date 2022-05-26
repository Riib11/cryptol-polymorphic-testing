# Combinatorial Generators

Polymorphic literal values can be constrained by typeclass-like constraints. For example

```
head : {n, a} (1 <= n) => [n]a -> a
head xs = xs@0
```

In order to test this function, need to be able to generate `n` such that `1 <= n`. For general boolean constraints like this, problem is undecidable. But since there are only a few constraints that Crytol supports, can actually just write a custom generator for each, in a combinatorial style.

In this case, there is a generator for `1`, a generator for `n`, and a generator for `_ <= _` which takes two generator and produces another generator.

# Constraints

The grammar for constraints on numeric literals variables (NLVs):

```
<constraint> ::= prime(<var>) | fin(<var>) | <expr> <rel> <expr>

<expr> ::= <var> | <nat> | <expr> <op2> <expr>

<op2> ::= + | - | * | ^^ | %

<rel> ::= < | > | =
```

# Linear constraints

In linear constraints, each variable is referenced exactly once. Then, we can treat each variable totally independently. 
<!-- This is almost totally useless in practice, but can be theoretically useful for deriving the general principles of a framekwork.  -->

# Nonlinear constraints

In nonlinear constraints, each variable can be referenced any number of times (in multiple separate constraints). For example:
```
(x <= y, y < z)
```
Having independent generators won't quite work here, since the result of one generator might depend on the result of another generator which might depend on the first generator.

Note that any set of nonlinear constraints can be converted to a set of linear constraints unioned with a set of equality constraints.
```
(x <= y, y < z) ~~>
(y1 = y2, x <= y1, y2 < z)
```
For each variable that appears more than once, replace each appearance with a unique instance, then introduce equality constraints between all of these instances. // TODO: should it produce all n^2 equalities (pairs), or just the minimal n (chain) making use of transitivity?

# Ranges

Once all constraints have been accounted for, often the available values will be constrained to a (perhaps filtered) range. This range is always bounded below by at least `0` (since type-level literals must be nats). Then there are two kinds of ranges:

- ranges that are _bounded above by a constant_:
  - usually the upper bound will be reasonably small //REVIEW
  - any reasonable distribution over a reasonably small range will do; could just default to the **negative exponential distribution** to be the same as not bounded above range // REVIEW
- ranges that are _bounded above by a function_:
  - the upper bound is a function of other variables, which have their own ranges
  - the distribution needs to be calculated based on the values sampled from the ranges on which the upper bound depends
- ranges that are _not bounded above_:
  - since upper bound is not specified, need to pick a reasonable one. 
    - Iavor mentioned that usually a couple thousand is more than sufficiently high, so lets do **2048** //REVIEW
  - To focus on low number, but include a few high numbers, use **negative exponential distribution** // REVIEW
