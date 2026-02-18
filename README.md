cl-hamt
=======

[![Build Status](https://travis-ci.org/danshapero/cl-hamt.svg?branch=master)](https://travis-ci.org/danshapero/cl-hamt)

This library provides purely functional dictionaries and sets in Common Lisp based on the hash array-mapped trie data structure.
The operations provided are:
```
size
lookup
insert
remove
reduce
filter
map
eq
```
The versions for sets and dictionaries are obtained by prepending `set-` or `dict-` to the above symbols, so for example to lookup a key in a set and dictionary you would call `set-lookup` and `dict-lookup` respectively.
An empty collection is created with the functions `empty-set` and `empty-dict`.
By default these use a fast built-in xxHash64-based mixer over `sxhash`; pass
` :hash-mode :keyed` to use a keyed SipHash mixer over `sxhash`, or
` :hash-mode :secure` to use SipHash over canonical safe object bytes.
You can also pass `:hash` to supply a custom hash function.

See the `examples/` directory for some usage examples of the library, or the unit tests.
Some benchmark code can be found in `test/benchmarks.lisp`.

Hash Quality Analysis
=====================

A standalone statistical hash analysis script is available at
`test/hash-stats.lisp`. It reports:
- unique hash count / collision count
- bucket occupancy min/max and chi-square
- bit-1 ratio across 64 output bits

Example:
```
sbcl --script test/hash-stats.lisp -- 50000 4096 20
```
Arguments are `N` samples, number of `buckets`, and number of `trials`
(defaults: `50000`, `4096`, `20`).


Implementation
==============

The data types in cl-hamt are implemented using the [hash array-mapped trie](https://idea.popcount.org/2012-07-25-introduction-to-hamt/) (HAMT) data structure, as found in the Clojure language.
HAMTs provide near-constant time search, insertion and removal and can be used as persistent data structures, i.e. all updates are non-destructive.

As the name suggests, hash array-mapped tries use hashing of the underlying data to store and retrieve it efficiently.
Consequently, any natural ordering on the data, e.g. lexicographic ordering of strings, natural ordering of integers, is not preserved in a HAMT.
When using `reduce` on a collection, the operation in question must not depend on the order in which the elements are accessed.
If the data are ordered and this ordering is important, a self-balancing binary tree may be a more appropriate data structure.
Additionally, one must provide an appropriate 64-bit hash function.
The built-in defaults are:
- `:fast` (default): xxHash64 over `sxhash`
- `:keyed`: SipHash-2-4 over `sxhash`
- `:secure`: SipHash-2-4 over canonical safe object bytes (no `print-object` dispatch)

Custom hash functions can be provided with the `:hash` keyword and are expected
to return an unsigned 64-bit integer.

The trie slice width is compile-time configurable in `src/util.lisp` via
`+slice-bits+` (set to `5` for 32-way nodes or `6` for 64-way nodes). Maximum
depth is derived from hash width and slice width at compile time.

While most operations on HAMTs have complexity logarithmic in the branching
factor (`2^+slice-bits+`) of the data structure, there is quite a bit of overhead.
HAMTs are probably less efficient for repeated operations on small-size sets and dictionaries than, say, a list or an association list.
