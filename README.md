HTestU
------

Framework for testing correctness of Pseudorandom Number Generators
(PRNGs) in Haskell.  Wrapper for a TestU01 framework as per paper
["TestU01: A C Library for Empirical Testing of Random Number Generators" by P. L'Ecuyer and R. Simard](http://simul.iro.umontreal.ca/testu01/tu01.html)

For examples of how to run the tests, read and build the exe
referenced in the cabal file either:

* via cabal

    > cabal configure -ftestKnownRNGs
    > cabal install

* via stack

    > stack build --flag htestu:testKnownRNGs

