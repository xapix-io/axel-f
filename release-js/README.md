# Axel-f

> Expressions language for Clojure(Script) and JavaScript inspired by Microsoft Excel ™

# Installation

Using npm:

```
npm i -g npm
npm i --save axel-f
```

In Node.js

```javascript
const {compile: compile, run: run, autocomplete: autocomplete} = require('axel-f');

# Precompile excel formula
compile("=SUM(1, 2, {5, 6, 8})");

# Execute formula as string
run("SUM(foo.bar[*].baz, {5, 6, 7})", {foo: {bar: [{baz: 1}, {baz: 2}]}});

# Execute precompiled formula
const formula = compile("=SUM(1, 2, {5, 6, 8})");
run(formula) # => 22

# Autocomplete ability
autocomplete("") # => [... All implemented functions ...]
autocomplete("CONCATENATE(") # => Description for function arguments including an index of current one
# [ { type: 'FNCALL',
#     value: 'CONCATENATE',
#     'current-arg': 0,
#     desc: 'Appends strings to one another.',
#     args:
#      [ { desc: 'The initial string.' },
#        { desc: 'More strings to append in sequence.',
#          opt: true,
#          repeatable: true } ] } ]

autocomplete("SUM(foo[*].", {foo: [ {bar: 1}, {baz: 2} ]}) # => Context suggestions
# [ { type: 'OBJREF', value: 'bar', desc: 'Field in the context' },
#   { type: 'OBJREF', value: 'baz', desc: 'Field in the context' } ]

autocomplete("SUM(foo[*].beer", {foo: [ {bar: 1}, {beer: 2}, {ber: 3} ]}) # => Fuzzy matching
# [ { type: 'OBJREF', value: 'beer', desc: 'Field in the context' },
#   { type: 'OBJREF', value: 'ber', desc: 'Field in the context' },
#   { type: 'OBJREF', value: 'bar', desc: 'Field in the context' } ]
```

Please check main [README](https://github.com/xapix-io/axel-f) for complete list of available features and functions.

# Copyright and License

Copyright © 2018 [Xapix GmbH](https://www.xapix.io/), and contributors

Distributed under the Eclipse Public License, the same as Clojure.
