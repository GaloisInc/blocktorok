# Blocktorok: Easy Structured Data Transformers

## Introduction

**Blocktorok** is a language ecosystem for expressing data schemas, encoding
data that matches these schemas, and transforming these data into arbitrary
targets defined by the user.

Blocktorok features simple syntax, static analysis, and the flexibility to
express data across many domains in ways natural to practitioners in those
domains.

The design of Blocktorok was motivated by problems in the usability of tools
for modeling physical phenomena: Labs often use custom tools, making
collaboration between groups difficult and tedious. With Blocktorok, a common
language can be agreed upon and individual labs can continue to use their own
tools simply by defining a transformer from the common language to those tools'
input languages. In particular, it is possible, through the use of multiple
transformers, for a single expression of a physics problem to be translated to
the language of sophisticated (but challenging to use) systems such as SU2 and
OpenFoam.

## Prerequisites

There are currently no binary releases of Blocktorok, so it must be built from
source.

In order to build Blocktorok, you must have an installation of the Glasgow
Haskell Compiler (GHC) and the `cabal` build system. The easiest way to install
these tools on most *nix platforms (including OSX) is through
[`ghcup`](https://www.haskell.org/ghcup/) - This tool allows for the management
of multiple GHC/`cabal` installations, and in most cases "just works."

In addition, if you plan to run any examples of SU2/OpenFoam output, an
installation of those tools is required:

* [SU2](https://su2code.github.io/)
* [OpenFoam](https://openfoam.org/)

Intallation/use instructions can be found at the linked resources.

## Building Blocktorok

To build Blocktorok, simply run `cabal build` from within the directory
containing this README. Any Haskell dependencies necessary will be fetched and
built automatically.

## Installing Blocktorok

From the directory containing this README, simply run
`cabal install blocktorok`. This will create a symbolic link to the executable
in your `.cabal/bin` directory; if this directory is on your `$PATH`, you will
be able to invoke `blocktorok` by its name alone. The following sections assume
this is the case.

## Running Blocktorok

Executing `blocktorok` at the command line results in this output:

```text
Missing: COMMAND

Usage: blocktorok COMMAND
  Work with Blocktorok data.
```

This isn't particularly helpful; more detailed information can be displayed by
passing the `-h` or `--help` flags. Here is the help output:

```text
blocktorok - A Blocktorok data transformer suite

Usage: blocktorok COMMAND
  Work with Blocktorok data.

Available options:
  -h,--help                Show this help text

Available commands:
  doc                      Generate documentation from a schema
  template                 Generate a data template from a schema
  build                    Run a transformer on data
```

Each of the available commands (`doc`, `template`, `build`) has its own
`--help` flag.

### `blocktorok doc`

```text
Usage: blocktorok doc FILE
  Generate documentation from a schema

Available options:
  FILE                     The schema to generate documentation from
  -h,--help                Show this help text
```

Given a schema `schema.ocs`, automatically generated documentation for it can
be displayed on standard output (and thus directed elsewhere) like so:

```bash
blocktorok doc schema.ocs
```

### `blocktorok template`

```text
Usage: blocktorok template FILE
  Generate a data template from a schema

Available options:
  FILE                     The schema to generate a data template from
  -h,--help                Show this help text
```

Given a schema `schema.ocs`, an automatically generated blank data template can
be displayed on standard output (and thus directed elsewhere) like so:

```bash
blocktorok template schema.ocs
```

### `blocktorok build`

```text
Usage: blocktorok build (-t|--transformer FILE) (-o|--output DIR) FILE
  Run a transformer on data

Available options:
  -t,--transformer FILE    The transformer to apply to the input data
  -o,--output DIR          The directory to send outputs to
  FILE                     The data to be transformed
  -h,--help                Show this help text
```

Given a transformer definition `transform.oct`, some data `data.blok`, and an
output directory name `out/` the compiler can be run like so:

```bash
blocktorok --transformer transform.oct --output out/ data.blok
```

Which will transform `data.blok` according to the rules in `transform.oct`,
placing any output files in the directory `out/`.

## Tests and Documentation

Within the directory `test_cases/` is a subdirectory named `automated`, which
contains a number of small examples of data and transformers to exercise the
compiler and get a feel for the language syntax.

To run an example, from a Bash-compatible shell, simply run:

```text
./run_test.sh <directory name of test case>
```

For example, to run the RPG battle example, you would type:

```text
./run_test.sh battle
```

Of particular interest are the tests `heated_rod_su2` and
`heated_rod_openfoam`, which use identical schemas and data inputs but
different transforms to produce very different simulation codes for the same
problem, demonstrating the power of the Blocktorok workflow.

Documentation for the Blocktorok schema and transformer languages can be found
in the `docs/` directory.
