hsRpal
======

The eventual goal of this project is to have a fully functioning RPAL
interpreter for COP5555, taught by Manuel Bermúdez at the University of Florida.

What is RPAL?
-------------

> **R**ight-reference **P**edagogic **A**lgorithmic **L**anguage

RPAL is a functional programming language derived from ML. It's pretty much just
esoteric at this point, but it makes for a good academic exercise.

Implementation Language
-----------------------

This interpreter is written in Haskell for a few reasons.

-   Haskell is like a better, more modern RPAL. The syntaxes are very similar,
    and there is much overlap in the involved concepts.
-   A parser lends itself to a purely functional programming language.
-   I've never used Haskell before, so this gives me an excuse to learn
    something new.

### Dependencies

- GHC (`ghc` on Debian)
- Text.Regex.TDFA (`libghc-regex-tdfa-dev` in Debian, `regex-tdfa` in Hackage)

Project Scope
-------------

The project is to be developed and delivered in two phases:

- Parser and AST generation only
- Full interpreter
