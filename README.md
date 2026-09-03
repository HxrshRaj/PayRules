# PayRules

A pure, typed authorization rules engine for payment transactions, in Haskell.

Given a transaction and an account context, it runs a pipeline of composable
authorization rules and returns an **approve / decline** decision with a
**reasoning trail** — which rule fired and why.

> **Status: early WIP.** The `Money` foundation is in place; the rule engine,
> rules, property tests, and CLI are being built on top. This README is a stub
> and will be rewritten once the project is complete so it documents what was
> actually built (including any bug that property-based testing caught).

## Build

```
stack build      # provisions GHC 9.6.6 via the lts-22.28 snapshot
stack test
```

`cabal build` also works against the hand-written `PayRules.cabal`.
