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

- `ConstraintKinds` to use constraints as type synonyms
- `DataKinds` to use data constructors as types
- `DefaultSignatures` to allow default implementations inside a class declaration for all or some of its methods
- `DeriveDataTypeable` for deriving
- `DeriveGeneric` for deriving
- `EmptyDataDecls` for empty data declarations like `data Foo`
- `FlexibleContexts` to relax some restrictions on the form of a type signature
- `FlexibleInstances` to relax some restrictions on the form of a class signature
- `FunctionalDependencies` to constrain the parameters of a multi-parameter class by stating that one of the parameters can be determined from the others
- `GADTs` to allow generalized ADTs
- `GeneralizedNewtypeDeriving` to derive any class through a newtype
- `InstanceSigs` to give type signatures in instances, which can be useful documentation
- `LambdaCase` to desugar `\ case` to `\ x -> case x of`
- `MultiParamTypeClasses` to have classes with multiple parameters
- `NamedFieldPuns` to bind local names to field names of a record
- `NoImplicitPrelude` to allow use of classy prelude
- `NoMonomorphismRestriction` to prevent the compiler from filling in free types with defaults
- `OverloadedStrings` to allow string literals to be interpreted as different string representations
- `PackageImports` to allow qualified imports
- `PatternSynonyms` to provide names to pattern matches
- `PolyKinds` to allow declarations to have kind variables, like `data Proxy (a :: k)`
- `QuasiQuotes` to help template haskell
- `RankNTypes` to put all variable declarations within the same `forall`
- `RecordWildCards` to allow wildcards in data types
- `ScopedTypeVariables` to allow usage of explicit forall type variables to be used in the body
- `StandaloneDeriving` to allow deriving after type declaration
- `TemplateHaskell` to turn on template haskell
- `TupleSections` to allow `(a,)` to desugar to `\ b -> (a, b)`
- `TypeApplications` to provide explicit type arguments to polymorphic functions
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
- `ekg` and associated libraries for statistics
- `lens` for high powered operations on data types
- `opaleye` to access a database
- `servant` for web applications

More high-powered libraries:
- `syb` for fancy recursion
- `vinyl` for HLists and the like

## General Style

In general, adhere to a certain flavor of style for a cohesive feel:

- Indentation level is 2 spaces
- Column width is 100
- Dealing with multiple lines:
    - When in doubt, adhere to Stylish Haskell
    - Multiline function calls should add an indentation level to any extra lines in order to distinguish those lines from surrounding code
    - Multiline function definitions should align arrows (`->`)
        - Function definition constraints may go on the same line as the function name
        - Start a newline and add an indent level (no need to be aligned under `::`)
    - Multiline lists should add a space after the list-opening character (`[` or `(`), align the first character of each line, and start each line
    - with a comma
    - Multiline expressions should have operators at the _beginning_ of each continued line, indented from the first.
        - Exception: place `$` at the end of the line, mostly.
    - Multiline imports should add a newline after the module name and indent (maintained by stylish-haskell)
- No wildcard imports except for ClassyPrelude
    - Okay in tests if you wildcard import the module you are testing
        - When you do this, import it in a separate section and call out the fact that you are doing it only because it's a test
        ```haskell
        module FooSpec where
        import ClassyPrelude
        import LiterallyAnythingElseDontYouDareWildcard ()
        -- imported wildcard because it's being tested
        import Foo
        ```
- In general, no orphan instances (sometimes, sometimes they may be necessary)
  - Okay in tests
  - Always write these in a separate module named `<ClassName>OrphanInstances.hs`
- Avoid importing multiple modules qualified under the same name (e.g. `import qualified Foo as F` and `import qualified Bar as F` together is bad)
- Record field names should be prefixed by the entire type name in camel case
    - If lenses are generated for this type, each record name should start with an underscore
    - An added advantage of declaring record types this way is that derived JSON instances do not have collisions
- Avoid `&` and the lens operators, e.g. `.~`.
    - _Exception_: certain APIs lead to very unwieldy code unless the lens operators are used, specifically swagger2. In that case we use them liberally.
- Avoid unnecessary parentheses
    - _Exception_: instance declarations that have constraints are easier to read if any constraints (or even a single constraint) are surrounded by parens. See example below.

### Examples

Some examples as a guide. These aren't rules, but use best judgement to write code that is readable and easy to understand.

#### `let` clauses

When using let clauses, try to define as many clauses as possible in the same block without reusing the let keyword. For pure functions, this is enforced by the compiler, but within a do block it is up to the programmer. For example:

```haskell
foo :: a -> b -> c -> m d
foo x y z = do
 -- NOT THIS:
 let bar = baz x
 let bin = qux y
 ...
 -- this instead:
 let bar = baz x
 bin = qux y
 ...
```

#### `where` clauses

Where clauses are nice when a function can be written concisely using functions that need not be at the module level. A simple example is a fold:

```haskell
foo :: [a] -> Map b c
foo = foldr makeMap mempty
 where
 makeMap = ...
```
Where clauses are also nice when using explicit type signatures.

Where clauses should add an indentation level to the `where` keyword, then a newline, then another indentation level, unless the function (or value) can be inlined.

#### Records

If the record type will be part of the "environment" for any code (i.e. with MonadReader), use `makeClassy ''MyType` to generate a `HasMyType` class (as well as lenses for the fields). Otherwise, most of the time it’s useful to generate lenses with `makeLenses ''MyType`.

The fields in the declaration of a record type should be aligned:

```haskell
data SomeReallyCoolType = SomeReallyCoolType
 { _someReallyCoolTypeName :: Text -- ^What name is for.
 , _someReallyCoolTypeDetailInfo :: Foo -- ^Something about detail.
 }
makeLenses ``SomeReallyCoolType
```

If you run out of space, the haddock comments can be moved to the line following each field. If so, all of them should be moved. But they need not be aligned when instantiating, because these tend to be indented and the rhs may be long:

```haskell
...
 pure $ SomeReallyCoolType
 { _someReallyCoolTypeName = foo bar baz $ flarp <$> mconcat zebras
 , _someReallyCoolTypeDetailInfo = anotherLongExpression of someValues
 }
```

#### Being point-free

Being point-free is great until it becomes confusing. For instance, don't abuse flip in favor of writing a function point-free. Operators can also be confusing in point-free syntax, as they often need extra parentheses. Here's a bad example of a point free function:

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

Provide an explicit type (or a comment!) when code is confusing. A type is preferable to a comment (if it’s equally explanatory) because a type is checked by the compiler.

#### Function types and constraints

In general, stay within the line limit, indent by 2 spaces when needed, and don't be too fussy:

```haskell
-- Keep it simple:
foo :: MySite site => Server site Foo

-- Break long lines:
foo :: MySite site
  => Foo -> Bar -> Baz -> Server site Bin

-- When there many constraints, use a space inside the "(" to line them up, and add a line break if it helps:
doAThing ::
  ( HasEnv r, HasRedisPool r, HasServerSettings r
  , MonadLogger m, MonadReader r m, MonadUnliftIO m )
  => Foo -> Bar -> Baz -> m Bin

-- Special case: always use parens with a constraint after "instance":
instance (HasSwagger sub) => HasSwagger (MyAuth :> sub) where
```

#### Breaking up long expressions

```haskell
-- Operators on the left, and indented:
"A long string "
  <> tshow foo <> ": "
  <> tshow bar

-- OK to indent creatively for better alignment:
Q.where_ ( x Q.^. FieldFoo Q.==. Q.val foo
     Q.&&. x Q.^. FieldBar Q.==. Q.val bar )

-- Consider an alternative if you want things aligned:
concat
  [ "A long string "
  , tshow foo , ": "
  , ...

-- $ is special:
fooSpec = describe "foo" $
  it "is awesome" $
    pure True

-- But you can also put it at the front if it works better
foo <- either throwIO pure
  . f (name <> " string") (object bar)
myFunction
  $ myValue
```

## Usage

When to use...

### `newtype` vs `data` vs `type`

newtypes are great for type safety. With `GeneralizedNewtypeDeriving` turned on, (almost) any class can be derived through a newtype. The type itself is erased at runtime, so there's no performance detriment to using these. Declarations should adhere to at least one of the following rules: either the `newtype` has one record, named `unConstructorName`, or there is no record name (just a constructor), but `makePrisms` is invoked for the type.

`data` is for constructors with multiple fields, ADTs, or GADTs. Always use data for types which use `deriveJSON` or similar to derive instances which depend on the field name, even if there is only one field. (This is in keeping with the rule about only naming that field `unConstructorName` for newtypes.) Best practice for using ADTs is to define a detail data type for each branch of the ADT so you get something like `data Foo = Bar BarDetail | Baz BazDetail`. This is especially useful for prisms (in the lens family).

type synonyms (aliases) are great for ascribing a specific name to a repeated union of types. With `ConstraintKinds` turned on, types can be used in class or instance declarations, like `type FooM a m = (Monad m, Foo a)` with `class FooM a m => Bar a m` where. Don't use a type alias with a simple type as in `type ImportantThing = Int`; instead use a newtype and enjoy the warm feeling of type safety.

### Lenses vs accessors

Lenses (and prisms, for ADTs) should be generated at compile time for any type that will be a part of any significant business logic in the code. Types that are only used in an outward-facing API, for example, need not generate lenses as they are typically only used at the edge of the server.

### Monad vs applicative

`Applicative` provides sequential sequential application (`<*>`) on a value, while `Monad` provides binding (`>>=`). Based on the type of `(>>=) :: m a (a m b) m b`, we can see that `Monad` requires an `a` to be present in order to compute `m b`. But with `(<*>) :: f (a b) f a f b`, effects are composed/combined independently of each other. An important consequence of this is that in validation, failures can accumulate. For this reason, consider using `Applicative` when parsing or validating. Finally, it's always useful to consider using the weakest constraint when writing code, so if a block can be written using `Applicative` instead of `Monad`, it could be useful to do so.

### Operator chain vs do-notation

Operator chains (i.e. `>>=`, `>>`, `>=>`) are useful for point-free expression across a few lines. If abused, they can lead to confusing code. When in doubt, use `do` or combine operations in a `let`/`in`/`where` block.

### A class

Classes are useful when operator overloading is needed, also known as ad-hoc polymorphism. Useful when many types have similar operations (like a simple database insert that always takes an entity and returns the entity plus its created key).

Take care when writing a class, and document any assumptions into laws to which the class should adhere. If possible, write property tests for such laws and expose them so alternative implementations can be tested.

### A type family

A type family is a type-level function that returns a type. Very useful when a class of functions returns the same kind of thing, and in such a case acts as a witness. A very powerful implementation of this is when it is used with functions on GADTs, so that the type family may be a witness to each GADT constructor's return type.

### Constraints vs transformers

Constraints are more flexible, as the only constraints that are needed for a function are the ones that are actually used. Monad transformers are much more static, as they require the valid monad stack.

### ExceptT vs EitherT

Always use `ExceptT`. It is older and more widely supported.

### `fail` vs `throwError` vs `error`

Old rule, used in some places: In general, try not to use exceptions (`fail`). Sometimes they are unavoidable, if `MonadError` (`throwError`) is not on the stack. `MonadError` is almost always preferable, since it is recoverable. In general an exception will be considered a server error.

New rule, used in some places: when `IO` (or `MonadIO`) is in the stack, it may be more sane to use exceptions (i.e. `throwIO`) because 1) some libraries already force it on us, 2) there is a useful semantics for what happens when exceptions are thrown asynchronously, and 3) something intelligent-sounding about masking and error handling. When throwing exceptions, always create an exception type which records some meaningful information and document where exceptions can be thrown, within reason.

Be aware of which rule is in force and be consistent. Consider switching to the new rule especially when doing more asynchronous IO.

It's OK to use fail only at the top level of a script or main program, for example when validating inputs.

Never, ever, ever use `error`. Exception: `error "not implemented"` is a convenient way to create a “hole” that will allow the program to run (unlike both `_` and `undefined`, which are both rejected by the compiler when `-Werror` is enabled). These should always be cleaned up before code is deployed, and usually before it is merged.

### `foldr` vs `foldl'` vs `foldl`

One can intuit some differences between these functions from the following examples:

- `foldr` preserves ordering while `foldl'` reverses the list (as would `foldl`).
```haskell
foldr (:) [] ['a', 'b', 'c']
"abc"

foldl' (flip (:)) [] ['a', 'b', 'c']
"cba"
```

- `foldl` is lazy and never finishes evaluation on an infinite list, but `foldr` does.
```haskell
foldl (\ xs x -> (x+1):xs) [] [(0 :: Int)..]
-- lazy version never exits

foldr (\ x xs -> (x+1):xs) [] [(0 :: Int)..]
-- all the things
```

- `foldl'` uses complexity proportional to its output, while foldr uses complexity at least as proportional to its input.
```haskell
:set +s
foldl' (+) 0 [(1 :: Int)..1000000000]
500000000500000000
(11.43 secs, 96,000,471,976 bytes)

foldr (+) 0 [(1 :: Int)..1000000000]
*** Exception: stack overflow
```

### `nub` vs `ordNub`

You should use `ordNub`/`ordNubBy` whenever possible. nub is an O(n^2) function that removes duplicate elements from a list. In particular, it keeps only the first occurrence of each element. `ordNub` is an O(n log n) version of the nub function that uses comparisons via `Ord` instead of `Eq`.

ClassyPrelude exports `ordNub`, `ordNubBy`, and `hashNub` (an O(n log n) function requiring `Hashable` and `Eq`).

See https://github.com/nh2/haskell-ordnub/blob/master/README.md#ordnub

### Type applications

The language option `TypeApplications` allows a more compact syntax when it's necessary to provide a type hint to the compiler. Instead of using `::`, provide a type preceded by `@` in argument position (with no space between the `@` and the type it decorates).

```haskell
encode $ toSchema (Proxy :: Proxy Text)
:set -XTypeApplications
encode $ toSchema $ Proxy @Text
encode $ toSchema $ Proxy @(Map Text Int)
```

### Pattern Synonyms

To language option `PatternSynonyms` allows naming of pattern matches.

Pattern synonyms are useful when you want to hide the representation of a datatype. For example, the containers package defines a type `Seq` representing finite lists. It is implemented as a special sort of tree, but the implementation is not exposed. Instead, the package defines pattern synonyms like `Empty` and `:<|` which allow you to match on a `Seq` as if it were a list:

```haskell
head :: Seq a -> Maybe a
head Empty = Nothing
head (x :<| _) = Just x
```

### Validation Applicatives

There are two validation libraries, `validation` and `either`.

### Default QuickCheck instances

Never use a default QuickCheck instance for `Text`.

It is okay to use `Test.QuickCheck.Instances.Time ()` for `UTCTime` instances, though there is a known issue around this in https://github.com/phadej/qc-instances/pull/13.

It is okay to use `Test.QuickCheck.Instances.Semigroup ()` to get `NonEmpty` instances.

Never import `Test.QuickCheck.Instances ()` wholesale.

## Gotchas

### `MonadResource`/`ResourceT`

`MonadResource` (and `MonadMask`, to some extent) operates similarly to a python context manager (aka a with statement). It's important to keep track of resources created within a `runResourceT` block because they will be cleaned up. A common example of this is `sinkCacheLength`, which can be used to read the number of bytes in a file before streaming it to S3. The behavior of `sinkCacheLength` is to "stream the input data into a temp file and count the number of bytes present. When complete, return a new `Source` reading from the temp file together with the length of the input in bytes." The temp file exists for the span of the resource block. Following are two (contrived) examples illustrating improper (first) and proper (second) use of this.

```haskell
let source = sourceLbs "any source can go here"
cacheLength <- runResourceT $ map (over _1 fromIntegral) $ runConduit $
  source .| sinkCacheLength
-- this won't work because the temp file doesn't exist out here
doSomethingToIt url cacheLength -- stream the thing into S3
```

### Algebraic Data Types

When defining an ADT like `data Foo = Bar | Baz`, it's important that the data constructors `Bar` and `Baz` for the type `Foo` take at unnamed arguments. Using named arguments can result in runtime exceptions from partial functions. The REPL is helpful in illustrating this.

```haskell
data Foo = Bar { unBar :: Int } | Baz
:t unBar
unBar :: Foo -> Int unBar $ Bar 1
1
unBar $ Baz
*** Exception: No match in record selector unBar
```

Because of this fact, never use record syntax with ADTs.

Furthermore, it's not inherently bad to declare `data Foo = Foo Int Int`, but as a matter of style it's good to give names to arguments. Instead of `data Foo = Foo Int Int` try d`ata Foo = Foo FooDetail | Bar` where `data FooDetail = FooDetail { fooDetailFirstInt :: Int, fooDetailSecondInt :: Int }`.

### ADT style

In general, write sum types like this:
- newline before =
- Align = and | at 2 spaces of indentation
- Docs for constructors indented 4 spaces (or if they are all short, they can all be on the same line as the constructor they pertain to.)
- Also align `deriving` at 2 spaces of indentation

```haskell
-- |Doc for Foo
data Foo
 = FooBlah
 -- ^Doc for FooBlah
 | FooBleh
 -- ^Doc for FooBleh
 | FooBlorp
 -- ^Doc for FooBlorp
 deriving (Bleep, Blorp, Bloop)
```

And in general, write product types like this:
- fields are prefixed by an underscore, and then the type name with the first letter lowercased. Fields are camel-cased.
- Align `{`, `|`, and `}` at 2 spaces of indentation
- Align the `::` (and note that `stylish-haskell` can do this for you)
- Put deriving on the same line as `}`

```haskell
-- |Doc for FooDetail
data FooDetail = FooDetail
  { _fooDetailOne :: SomeType
  -- ^Doc for fooDetailOne
  , _fooDetailAnotherDetal :: SomeOtherType
  -- ^Doc for fooDetailAnotherDetail
  } deriving (Bleep, Blorp, Bloop)
```

Typically, group such data definitions near the top of the module. After the data declarations, there should typically be a group of clauses to derive any needed instances, such as `makePrisms ''Foo` and `makeLenses FooDetail`. Typically, `makePrisms` for all sum types, and `makeLenses` for all product types. As mentioned above, use `makeClassy` instead if the product type is an "env" type.
