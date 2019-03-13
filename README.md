# Axel-f [![codecov](https://codecov.io/gh/xapix-io/axel-f/branch/master/graph/badge.svg)](https://codecov.io/gh/xapix-io/axel-f) [![CircleCI](https://circleci.com/gh/xapix-io/axel-f/tree/master.svg?style=svg)](https://circleci.com/gh/xapix-io/axel-f/tree/master) [![cljdoc badge](https://cljdoc.org/badge/io.xapix/axel-f)](https://cljdoc.org/d/io.xapix/axel-f/CURRENT)

> Expressions language for Clojure(Script) and JavaScript inspired by Microsoft Excel ™

# Rationale

In some applications, the lion's share of business logic is concentrated in dynamic expressions. Often they are not even part of the code base and are stored in a database, files, or somewhere in the cloud. And sometimes these expressions need to be accessed non-technical personnel, like managers or call centers operators. Making them learn clojure or python is not fair. This library is designed to combine the best of two completely different worlds: the most understandable syntax from Excel and the speed of Clojure.

# Installation

## Clojure(Script)

* **Leiningen** : `[io.xapix/axel-f "1.0.2"]`
* **Boot**: `(set-env! :dependencies #(conj % [io.xapix/axel-f "1.0.2"]))`
* **deps.edn**: `{:deps {io.xapix/axel-f {:mvn/version "1.0.2"}}}`

## JavaScript

[Please checkout the documentation for JavaScript package](https://github.com/xapix-io/axel-f/tree/master/release-js)

# TL;DR

```clojure
(require '[axel-f.core :as axel-f])

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

;; axel-f.core/analyze function returns used variables
(axel-f/analyze "SUM(1, 2, AVERAGE({4,5,6}), foo.bar, foo.baz[*].x)")
;; => {:vars (("foo" "bar") ("foo" "baz" "*" "x"))}
```

# Difference from Excel

* No cell-references or reference operations.

# Object references

In addition to a formula, the run function can accept execution context as a second argument. Context can be any valid Clojure(Script) object. In the formula you can select the data from context by using object reference operators:

* Dot reference operator for access nested data: `foo.bar.baz`
* Array reference operator for access data in vector: `foo[*].bar`
  * `foo[*][*]` we support nested vectors (vector of vectors of vectors ...)
  * and objects in vectors `foo[*].bar[*].baz`
  * it is possible to use indexes to get the data inside of arrays: `foo[1].bar`
  * index can be computed on the fly: `foo[SUM(x, 10)].bar`
* `_` can be used as a self reference. When used in special functions (MAP, FILTER, SORT) will reference single item of collection.
* field reference can have any character except space, single/double quote, dot, comma, opening/closing square/round brackets and operators
  * fields with mentioned symbols inside must be quoted by wrapping into `#''` or just a string: `#'bar > baz'[0].foo` or `'foo -> bar'.baz`
  * some functions such as `FILTER` or `SORT` possibly can return nested data structure and this data can be used as a root reference object: `FILTER(_.x = 1, _)[0].x` with context `[{:x 2} {:x 1} {:x 3}]` returns `1` as expected.

# Data types

- [x] Booleans as in Excel (TRUE/FALSE). In addition axel-f understands `True/False/true/false`
- [x] Numbers (Integers, Floats, Exponential form)
- [x] Strings in double or single quotes. (`'Some String'`, `"Some String"`)
- [x] Arrays. Any data in curly brackets (`{1, 2 TRUE}`)
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

# Copyright and License

Copyright © 2018 [Xapix GmbH](https://www.xapix.io/), and contributors

Distributed under the Eclipse Public License, the same as Clojure.
