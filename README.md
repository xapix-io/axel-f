# Axel-f [![codecov](https://codecov.io/gh/xapix-io/axel-f/branch/master/graph/badge.svg)](https://codecov.io/gh/xapix-io/axel-f) [![cljdoc badge](https://cljdoc.org/badge/io.xapix/axel-f)](https://cljdoc.org/d/io.xapix/axel-f/CURRENT) ![Code Style (clj-kondo)](https://github.com/xapix-io/axel-f/workflows/.github/workflows/lint.yml/badge.svg) ![Tests](https://github.com/xapix-io/axel-f/workflows/.github/workflows/tests.yml/badge.svg)

> Expressions language for Clojure(Script) and JavaScript inspired by Microsoft Excel ™

# Rationale

In some applications, the lion's share of business logic is concentrated in dynamic expressions. Often they are not even part of the code base and are stored in a database, files, or somewhere in the cloud. And sometimes these expressions need to be accessed non-technical personnel, like managers or call centers operators. Making them learn clojure or python is not fair. This library is designed to combine the best of two completely different worlds: the most understandable syntax from Excel and the speed of Clojure.

# Installation

## Clojure(Script)

* **Leiningen** : `[io.xapix/axel-f "2.0.11"]`
* **Boot**: `(set-env! :dependencies #(conj % [io.xapix/axel-f "2.0.11"]))`
* **deps.edn**: `{:deps {io.xapix/axel-f {:mvn/version "2.0.11"}}}`

## JavaScript

[Please checkout the documentation for JavaScript package](https://github.com/xapix-io/axel-f/tree/master/release-js)

# TL;DR

```clojure
(require '[axel-f.excel :as axel-f])

;; Compile Excel-like formula into executable function
(def foo (axel-f/compile "AVERAGE({1, 2, 3})"))

(foo)
;; => 2

((axel-f/compile "SUM(1, 2, AVERAGE({4,5,6}), foo.bar, foo.baz[*].x)")
            {:foo {:bar 1
                   :baz [{:x 5}
                         {:x 7}
                         {:x 8}]}})
;; => 29

;; metadata of compiled functions has information about used variables
(meta (axel-f/compile "SUM(1, 2, AVERAGE({4,5,6}), foo.bar, foo.baz[*].x)"))
;; => {:free-variables (("foo" "bar") ("foo" "baz" "*" "x")) ... }
```

# Difference from Excel

* No cell-references or reference operations.
* Extra functions to work with lists such as `MAP`, `FILTER`, `SORT`, `LENGTH`, `CONCAT`
* Lambda functions! `FN(x, y, x + y)` where all but last arguments are arglist, last - lambda's body with local bindings.

# Object references

In addition to a formula, the run function can accept execution context as a second argument. Context can be any valid Clojure(Script) object. In the formula you can select the data from context by using object reference operators:

* Dot reference operator for access nested data: `foo.bar.baz`
* Single string can be used as a reference by using dot character as a prefix `.'some string with spaces'`
* Array reference operator for access data in vector: `foo[*].bar`
  * `foo[*][*]` we support nested vectors (vector of vectors of vectors ...)
  * and objects in vectors `foo[*].bar[*].baz`
  * it is possible to use indexes to get the data inside of arrays: `foo[1].bar`
  * index can be computed on the fly: `foo[SUM(x, 10)].bar`
* field reference can have any character except space, single/double quote, dot, comma, opening/closing square/round brackets and operators
  * fields with mentioned symbols inside must be quoted by wrapping into string literals: `'bar > baz'[0].foo` or `'foo -> bar'.baz`
  * some functions such as `FILTER` or `SORT` possibly can return nested data structure and this data can be used as a root reference object: `FILTER(FN(item, item.x), _)[0].x` with context `[{:x 2} {:x 1} {:x 3}]` returns `1` as expected.

# Data types

- [x] Null as in Excel (NULL).
- [x] Booleans as in Excel (TRUE/FALSE). In addition axel-f understands `True/False/true/false`
- [x] Numbers (Integers, Floats, Exponential form)
- [x] Strings in double or single quotes. (`'Some String'`, `"Some String"`)
- [x] Arrays. Any data in curly brackets (`{1, 2, TRUE}`)
- [ ] Date
- [x] Excel Error types
- [ ] Geospatial Data

# Operators

Any expression can be used as an operand for any operator. axel-f has the same operator precedence as in Excel. To change a precendence of operators you can use round brackets (`(2+2)*2` => 8)

## Unary

- [x] Sign operator (`-/+`, eg. "-1") Can be used for coercing boolean into number (`--TRUE` => 1)
- [x] Percent operator (`2.4%`) axel-f uses float as replacement (`2.4%` => 0.024).

## Binary

- [x] Additive operators (`+/-`)
- [x] Multiplicative operators (`* and /`)
- [x] Comparison operators
  - more (`>`)
  - more or equal (`>=`)
  - less (`<`)
  - less or equal (`<=`)
  - equal (`=`)
  - not equal (`<>`)
- [x] Exponential operator (`^`)
- [x] Concatenate operator (`&`)

# Implemented functions

Please check the [wiki page](https://github.com/xapix-io/axel-f/wiki)

# Changes between 1.0.6 and 2.0.0

## Namespaces (!BREAKING!)

* axel-f.core => axle-f.excel , for ability to have separate (not just excel-like) extensions for axel-f core
* axle-f.analyze was replaced by metadata attached to compiled AST

## Working with collections (!BREAKING!)

* `MAP`, `FILTER`, `SORT` no longer accept reference as a first argument. Use lambda function instead.
* New function `CONCAT` to concatenate elements of multiple collections.

## Extensions (!BREAKING!)

To be more data-driven `def-excel-fn` was replaced by providing extra context to
`compile` function. It must be a map of token -> fn pairs.

## Local scope

New special function `WITH` can be used to create local bindings.

E.g.

```
WITH(x, 1,
     y, 2,
     x + y)
=> 3
```

Lambdas also supported here

```
WITH(foo, FN(x, x + 2),
     MAP(foo, {5, 6, 7}))
=> [7, 8, 9]
```

# Copyright and License

Copyright © 2018 [Xapix GmbH](https://www.xapix.io/), and contributors

Distributed under the Eclipse Public License, the same as Clojure.
