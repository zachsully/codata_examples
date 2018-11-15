# A Brief Description of Examples
---

## GHC Language Extension
---

The language extension can be enabled with the pragma {-# LANGUAGE Copatterns
#-}. The syntax for introducing codata types is essentially identical to that of
GADTs. Since the implementation is all done in the parser and observations just
overload the function application syntax, observations are names
"obs_<observation name>". For instance, the "Fst" observations for a with type
will be applied to an object "w" as "obs_Fst w".

Code:

* "Examples_From_Hughes_WFPM.hs"
* "Fib.hs"
* "Primes.hs"

## DL Language
---

See "dl/README.md" for invocation details.

Code:

* "fib100.dl"
* many small examples in "dl/examples/source"
