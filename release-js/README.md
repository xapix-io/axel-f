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
const {compile: compile, run: run} = require('axel-f');

# Precompile excel formula
compile("=SUM(1, 2, {5, 6, 8})");

# Execute formula as string
run("SUM(foo.bar[*].baz, {5, 6, 7})", {foo: {bar: [{baz: 1}, {baz: 2}]}});

# Execute precompiled formula
const formula = compile("=SUM(1, 2, {5, 6, 8})");
run(formula) # => 22
```

Please check main [README](https://github.com/xapix-io/axel-f) for complete list of available features and functions.

# Copyright and License

Copyright © 2018 [Xapix GmbH](https://www.xapix.io/), and contributors

Distributed under the Eclipse Public License, the same as Clojure.
