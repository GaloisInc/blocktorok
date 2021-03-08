# The LINK Programming Language

## Introduction

LINK is a domain-specific language (DSL) used to express multi-physics
problems. LINK combines simple and intuitive syntax, static analysis, and
sophisticated backend systems (such as SU2 and OpenFoam) to provide users with
what's necessary to confidently compose physical models.

## Prerequisites

There are currently no binary released of LINK, so it must be built from
source.

In order to build LINK, you must have an installation of the Glasgow Haskell
Compiler (GHC) and the `cabal` build system. The easiest way to install these
tools on most *nix platforms (including OSX) is through
[`ghcup`](https://www.haskell.org/ghcup/) - This tool allows for the management
of multiple GHC/`cabal` installations, and in most cases "just works."

In addition, to run the compiled output, an installation of
[SU2](https://su2code.github.io/) is required. Instructions can be found at the
linked resource.

## Building LINK

To build LINK, simply run `cabal build` from within the directory containing
this README. Any Haskell dependencies necessary will be fetched and built
automatically.

## Running LINK

Because LINK is in a prototype state, it is best and easiest to run it using
`cabal`.

Executing `cabal run steel` within the directory containing this README results
in this output:

```bash
Missing: FILES... (-o|--output FILE)

Usage: steel FILES... (-o|--output FILE)
  Compile a LINK program
```

More detailed help can be displayed by executing `cabal run steel -- -h` or
`cabal run steel -- --help`. The `--` passes the flags following to the
LINK executable rather than `cabal`. Here is the help output:

```text
steel - A LINK compiler

Usage: steel FILES... (-o|--output FILE)
  Compile a LINK program

Available options:
  FILES...                 The LINK sources to be compiled
  -o,--output FILE         The file to output to
  -h, --help               Show this help text
```

So, given a collection of LINK sources (say `a.steel`, `b.steel`, `c.steel`),
and a desired output file `out.cfg`, the compiler can be run like so:

```bash
cabal run steel -- a.steel b.steel c.steel --output out.cfg
```

This will link and compile the source files, dumping the output in `out.cfg`.

## Samples and Documentation

The `test_cases/heat_transfer_rod/su2` documentary contains some LINK sources
that will successfully link and compile - The sources are identical as this is
simply a proof-of-concept for some of the compiler features. The problem
represented is a single-model heat conduction problem along a simple rod.

Documentation for the LINK language can be found in the report distributed with
this code release.
