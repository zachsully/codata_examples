# Codata Examples

This repo contains "practical" examples of codata usage for two different
implememntations of codata.

## Haskell Language Extension

The compiler for these examples is found at
[https://github.com/zachsully/ghc/tree/codata-macro]. It is a fork of GHC 8.4.

The language extension can be enabled with the pragma
`{-# LANGUAGE Copatterns #-}`. The syntax for introducing codata types is
essentially identical to that of GADTs. Since the implementation is all done in
the parser and observations just overload the function application syntax,
observations are names `obs_<observation name>`. For instance, the `Fst`
observations for a with type will be applied to an object `w` as `obs_Fst w`.

Examples:

* `Examples_From_Hughes_WFPM.hs` - Examples from Hughes "Why Functional
  Programming Matters"
* `Fib.hs` - A standalone application for computing the nth Fibonacci number
* `Primes.hs` - A standalone application for computing the nth prime number
* `FileSystem.hs` - A small filesystem using indexed codata for controlling
    access to read/write operations
* `Set.hs` - Sets represented as codata

## Prototype Language

The compiler for these examples is at [https://github.com/zachsully/dl].

Examples:
* `fib100.dl` - A program computing the 100th Fibonacci number
* many small examples in `dl/examples/source`
