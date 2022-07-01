# Spec

## Constraints

The user provides a list of constraints defined by the following grammar:

```
<c>   ::= <e> <r> <e> | <tc>(<var>)
<e>   ::= <o1> <e> | <e> <o2> <e>
<o1>  ::= -
<o2>  ::= + | - | * | / | ^ | %
<r>   ::= = | <= | >=
<tc>  ::= fin | prime
```

## Basic Constraints

The following grammar encodes the subset of _basic constraints_ handled by this
solver/sampler.

```
<bc> ::= <be> <r> <be> | <tc>(<var>)
<be> ::= <Q>*x1 + ... + <Q>*xN + <Q>
```

A user-provided constraint is processed into some basic consraints via the
following rewrites:

<!-- TODO: show how to handle mod -->
```
...
```


The list of relation constraints is processed into a system of linear equations,
represented as a matrix over domain `Q`, where each relation constraint is
converted to a row of the (augmented) matrix in the following way:

```
{a1*x1 + ... + aN*xN + c = b1*x1 + ... + bN*xN + c'} ~~>
{(a1 - b1)*x1 + ... + (aN - bN)*xN = c' - c} ~~>
[ (a1 - b1) ... (aN - bN) | (c' - c) ]

{a1*x1 + ... + aN*xN + c <= b1*x1 + ... + bN*xN + c'} ~~>
{(a1 - b1)*x1 + ... + (aN - bN)*xN + x{N+1} = c' - c} ~~>
[ (a1 - b1) ... (aN - bN) 1 | (c' - c) ]

{a1*x1 + ... + aN*xN + c >= b1*x1 + ... + bN*xN + c'} ~~>
{(a1 - b1)*x1 + ... + (aN - bN)*xN + (-1)x{N+1} = c' - c} ~~>
[ (a1 - b1) ... (aN - bN) (-1) | (c' - c) ]
```

This matrix is solved via Gaussian elimination. The rows of the resulting matrix
are processed into equalities and inequalities via the following
pattern-matchings on each row I:

```
[ 0 ... 0 1 a{J+1} ... aN | c ] ~~>
{ xJ = -a{J+1}*x{J+1} + ... + -aN*xN
, xK1 <=     (xL1*aL1 + ... + xLO*xLO) / aK1
, xK2 <=   (((xL1*aL1 + ... + xLO*xLO) / aK1) - 1*xK1) / aK2
, ...
, xKM <= (((((xL1*aL1 + ... + xLO*xLO) / aK1) - 1*xK1) / aK2) ... ) / aKM
}
  where 
    xK1, ..., xKM are the variables with negative coefficients
    xL1, ..., xLO are the variables with positive coefficients
```
Due to the form of a Gaussian-eliminated matrix, it is ensured that any variable
xJ will be on the LHS of at most equality. The inequalities ensure that,
whenever a term is subtracted, that the subtracted quantity is less than the
sampled quantity it is being subtracted from. The ordering of sampling for the
subtracted terms does not affect their distribution [proof], so they are sampled
one at a time in arbitrary order.

These equalities and inequalities, along with the original typeclass constraints
are collected.

## Sampling

The collected equalities, inequalities, and typeclass constraints are sampled
over by the following algorithm:

```
Initialize upper bounds.
  Each variable's upper bound is initialized as infinity.
  Each variable that is constrained by fin or prime updates its upper bound with MAX_FIN.
  For each  inequality of the form {xJ <= a{J+1}*x{J+1} + ... + aN*xN + c} in the constraints, the variable xJ updates its upper bound to the minimum of the  current upper bound and {a{J+1}*x{J+1} + ... + aN*xN + c}. 
  For each equality of the form {xJ = e}, the variable xJ is assigned to e and does not have an upper bound.
Sample variables.
  For each variable xJ:
    If xJ has an upper bound:
      Sample or lookup the necesssary variables and then evaluate its upper bound to v.
      If v = infinity:
        Nondeterministically branch on assignments of xJ to samples from an exponential distribution from 0 to MAX_FIN, and a single sample of infinity.
      Else:
        Nondeterministically branch on assignments of xJ to samples from a uniform distribution from 0 to v.
        If xJ is constrained by prime, then only sample prime values from this distribution.
    Otherwise:
      xJ must be equal to an expression e.
      Sample or lookup the necessary variables and then evaluate the expression to v.
      If xJ is constrainted by prime and v is not prime, then abandon this nondeterministic branch.
      Assign xJ to v.
```