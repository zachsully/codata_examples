# Codata Examples

This repo contains "practical" examples of codata usage for two different
implememntations of codata.

## Haskell Language Extension

The compiler for these examples is found at
[https://github.com/zachsully/ghc/codata-macro]. It is a fork of GHC 7.4.

The language extension can be enabled with the pragma
`{-# LANGUAGE Copatterns #-}`. The syntax for introducing codata types is
essentially identical to that of GADTs. Since the implementation is all done in
the parser and observations just overload the function application syntax,
observations are names `obs_<observation name>`. For instance, the `Fst`
observations for a with type will be applied to an object `w` as `obs_Fst w`.

Examples:

* `Examples_From_Hughes_WFPM.hs`
* `Fib.hs`
* `Primes.hs`

## Prototype Language

The compiler for these examples is at [https://github.com/zachsully/dl].

Examples:
* "fib100.dl"
* many small examples in "dl/examples/source"
