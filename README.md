Based on the flattening transformation, as presented in http://manticore.cs.uchicago.edu/papers/ppopp13-flat.pdf

We wish to rigorously define the operations necessary to move between a source term language and a target term language in which the target language operates on flattened terms, consisting of the same source terms but with arrays flattened.
Although this is relevant for the internal details of the Manticore compiler, it should also demonstrate a rudimentary and non-optimized implementation of the flattening transformation.

We operate in terms of three distinct term languages:
Terms -- unqualified terms are source terms, and allow for any array structures
Nested Flat Terms -- these are an intermediate representation which accomodate both flattened arrays and nested arrays
Flat Terms -- these are strictly flat terms, and do not allow for arrays of arrays, only farrays

Hybrid flattening can be understood as a partial transformation in which the Nested Flat -> Flat step is not applied, but the nested flat representation (potentially in multiple forms) is kept in hand and used as the compiler judges to be appropriate.
