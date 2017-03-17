# Haskell Style Guide

## Introduction

This document is intended to be used as a reference for Haskell style and usage. It covers a range
of topics, from libraries and language features to style and usage.

## Build System

- Stack with Hpack is build system of choice. On a per-project basis, this permits concise
  `package.yaml` files that are generated into version-compatible `.cabal` files.

### Language Features

#### Compiler Options

- `-Wall`, `-Werror`, and `-fwarn-tabs` for warnings
- `-threaded` (executables only) for the threaded runtime
- `-02` for runtime performance (recommended), though it produces slower compilation times
- `-rtsopts`, `-with-rtsopts=-N`, and `-with-rtsopts=-T` (executables only) for enabling runtime
  options, using all processors, and showing runtime statistics, respectively

#### Pragmas

Pragmas to use anytime (and should be turned on by default):

- `DataKinds` to use data constructors as types
- `DeriveDataTypeable` for deriving
- `DeriveGeneric` for deriving
- `EmptyDataDecls` for empty data declarations like `data Foo`
- `FlexibleContexts` to relax some restrictions on the form of a type signature
- `FlexibleInstances` to relax some restrictions on the form of a class signature
- `GADTs` to allow generalized ADTs
- `GeneralizedNewtypeDeriving` to derive any class through a newtype
- `LambdaCase` to desugar `\ case` to `\ x -> case x of`
- `MultiParamTypeClasses` to have classes with multiple parameters
- `NamedFieldPuns` to bind local names to field names of a record
- `NoImplicitPrelude` to allow use of classy prelude
- `NoMonomorphismRestriction` to prevent the compiler from filling in free types with defaults
- `OverloadedStrings` to allow string literals to be interpreted as different string representations
- `PackageImports` to allow qualified imports
- `PolyKinds` to allow declarations to have kind variables, like `data Proxy (a :: k)`
- `QuasiQuotes` to help template haskell
- `RankNTypes` to put all variable declarations within the same `forall`
- `RecordWildCards` to allow wildcards in data types
- `ScopedTypeVariables` to allow usage of explicit `forall` type variables to be used in the body
- `StandaloneDeriving` to allow deriving after type declaration
- `TemplateHaskell` to turn on template haskell
- `TupleSections` to allow `(a,)` to desugar to `\ b -> (a, b)`
- `TypeFamilies` to allow type families
- `TypeOperators` to allow types as operators
- `ViewPatterns` to allow function application on values at the time they are unwrapped

Pragmas to use sparingly:

- `UndecidableInstances` (see [this](https://www.reddit.com/r/haskell/comments/5zjwym/when_is_undecidableinstances_okay_to_use/) explanation for details)

Pragmas to probably panic about:

- `OverlappingInstances`

### Libraries

Essential libraries and frameworks:

- `aeson` for JSON parsing and serialization
- `classy-prelude` like prelude, but way better
- `esqueleto` for high-powered postgres joins
- `ekg` and associated libraries for statistics
- `lens` for high powered operations on data types
- `persistent` to access a database (shipped with yesod)
- `yesod` for web applications

## General Style

In general, adhere to a certain flavor of style for a cohesive feel:

- Indentation level is 2 spaces
- Column width is 100
- Dealing with multiple lines:
  - Multiline function calls should add an indentation level to any extra lines in order to
    distinguish those lines from surrounding code
  - Multiline function definitions should align arrows (`->`)
  - Multiline lists should add a space after the list-opening character (`[` or `(`), align the
    first character of each line, and start each line with a comma
  - Multiline imports should add a newline after the module name and indent
- No wildcard imports except for `ClassyPrelude`
  - Okay in tests if you wildcard import the module you are testing
- In general, no orphan instances (sometimes, _sometimes_ they may be necessary)
  - Okay in tests
  - Always write these in a separate module named `<ClassName>OrphanInstances.hs`
- Try to stay away from importing multiple modules qualified under the same name unless they make
  sense (`import qualified Foo as F` and `import qualified Bar as F` together is bad)
- Record names should contain the entire type name in camel case
  - If lenses are generated for this type, each record name should start with an underscore
  - An added advantage of declaring record types this way is that derived JSON instances do not have
    collisions

### Examples

Some examples as a guide. These aren't rules, but use best judgement to write code that is readable
and easy to understand.

#### `let` clauses

When using `let` clauses, try to define as many clauses as possible in the same block without
reusing the `let` keyword. For pure functions, this is enforced by the compiler, but within a `do`
block it is up to the programmer. Try to avoid this:

```haskell
foo :: a -> b -> c -> m d
foo x y z = do
  let bar = baz x
  let bin = qux y
  ...
```

#### `where` clauses

`Where` clauses are nice when a function can be written concisely using functions that need not be
at the module level. A simple example is a fold:

```haskell
foo :: [a] -> Map b c
foo = foldr makeMap mempty
  where
    makeMap = ...
```

`Where` clauses are also nice when using explicit type signatures.

`Where` clauses should add an indentation level to the `where` keyword, then a newline, then another
indentation level, unless the function (or value) can be inlined.

#### Being point-free

Being point-free is great until it becomes confusing. For instance, don't abuse `flip` in favor of
writing a function point-free. Operators can also be confusing in point-free syntax, as they often
need extra parentheses. Here's a bad example of a point free function:

```haskell
foo :: a -> b -> c -> Either Text d
foo x y = either ((<> " messed up") . ("this value is " <>)) id . flip (flip (bar x) y)
```

This is better:

```haskell
foo :: a -> b -> c -> Either Text d
foo x y z = either (\ msg -> "this value is " <> msg <> " messed up") id $ bar z x y
```

#### Providing types that _could_ be inferred

Provide an explicit type (or a comment!) when code is confusing.

## Usage

When to use...

### `newtype` vs `data` vs `type`

`newtypes` are great for type safety. With `GeneralizedNewtypeDeriving` turned on, (almost) any
class can be derived through a `newtype`. The type itself is erased at runtime, so there's no
performance detriment to using these. Declarations should adhere to at least one of the following
rules: either the newtype has one record, named `unConstructorName`, _or_ there is no record name
(just a constructor), but `makePrisms` is invoked for the type.

`data` is for constructors with multiple fields, ADTs, or GADTs. Best practice for using ADTs is to
define a detail `data` type for each branch of the ADT so you get something like `data Foo = Bar
BarDetail | Baz BazDetail`. This is especially useful for prisms (in the lens family).

`type` synonyms (aliases) are great for ascribing a specific name to a repeated union of types. With
`ConstraintKinds` turned on, types can be used in `class` or `instance` declarations, like `type
FooM a m = (Monad m, Foo a)` with `class FooM a m => Bar a m where`.

### Lenses vs accessors

Lenses (and prisms, for ADTs) should be generated at compile time for any type that will be a part
of any significant business logic in the code. Types that are only used in an outward-facing API,
for example, need not generate lenses as they are typically only used at the edge of the server.

### Monad vs applicative

`Applicative` provides sequential sequential application (`<*>`) on a value, while `Monad` provides
binding (`>>=`) and does not guarantee sequential execution of code. In general, use `Applicative`
if the same value will be used in multiple computations (like `MonadReader`), or sequential
application is necessary (parsing).

### Operator chain vs do-notation

Operator chains (i.e. `>>=`, `>>`, `>=>`) are useful for point-free expression across a few lines.
If abused, they can lead to confusing code. When in doubt, use `do` or combine operations in a
`let`/`in`/`where` block.

### A class

Classes are useful when operator overloading is needed, also known as ad-hoc polymorphism. Useful
when many types have similar operations (like a simple database `insert` that always takes an entity
and returns the entity plus its created key).

Take care when writing a class, and document any assumptions into laws to which the class should
adhere. If possible, write property tests for such laws and expose them so alternative
implementations can be tested.

### A type family

A type family is a type-level function that returns a type. Very useful when a class of functions
returns the same kind of thing, and in such a case acts as a witness. A very powerful implementation
of this is when it is used with functions on GADTs, so that the type family may be a witness to each
GADT constructor's return type.

### Constraints vs transformers

Constraints are more flexible, as the only constraints that are needed for a function are the ones
that are actually used. Monad transformers are much more static, as they require the valid monad
stack.

### ExceptT vs EitherT

Always use `ExceptT`. It is older and more widely supported.

### `fail` vs `throwError` vs `error`

In general, try not to use exceptions (`fail`). Sometimes they are unavoidable, if `MonadError`
(`throwError`) is not on the stack. `MonadError` is almost always preferable, since it is
recoverable. In general an exception will be considered a server error.

Never, ever, _ever_ use `error`.
