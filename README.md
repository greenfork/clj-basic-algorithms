# Basic algorithms in Clojure

This is a collection of data structures and algorithms that are essential and
not so essential in the modern era. You are free to use any of these without
any obligations.

Why reinvent the wheel, Clojure is built on some of these structures?! Hehe, it is a good question.

# Available structures and algorithms

- [AVL tree][1] -- self-balancing binary search tree, O(log n) operations, on
  average faster than Red-Black tree on lookup but slower on modifications.
- [Binary search tree][2] -- most basic data structure. Also contains
  benchmarks on different implementations of the tree.
- [Index hashing][4] -- a collection of non-cryptographic algorithms used
  mainly for indexing. Includes Bernstein, FNV, One at a Time, Zobrist,
  Murmur3.
- [Hash Table][5] -- an array-like structure allowing for arbitrary indexes
  instead of just integers such as strings, keywords etc. Occupies more space
  but provides nearly O(1) lookup time.
- [Number generation][7] -- methods for various number generation algorithms.
  Includes Box-Muller-Marsaglia polar method (sample from normal
  distribution), Sample from Geometric Distribution.
- [Red-Black tree][3] -- self-balancing binary search tree, O(log n)
  operations, on average faster than AVL tree on modifications but slower on
  lookup.
- [Sorting][6] -- a collection of sorting algorithms. Includes bubble sort.

[1]: https://github.com/greenfork/clj-basic-algorithms/blob/master/src/clj_basic_algorithms/AVL_tree
[2]: https://github.com/greenfork/clj-basic-algorithms/tree/master/src/clj_basic_algorithms/binary_search_tree
[3]: https://github.com/greenfork/clj-basic-algorithms/tree/master/src/clj_basic_algorithms/redblack_tree
[4]: https://github.com/greenfork/clj-basic-algorithms/tree/master/src/clj_basic_algorithms/index_hashing
[5]: https://github.com/greenfork/clj-basic-algorithms/tree/master/src/clj_basic_algorithms/hash_table
[6]: https://github.com/greenfork/clj-basic-algorithms/tree/master/src/clj_basic_algorithms/sorting
[7]: https://github.com/greenfork/clj-basic-algorithms/tree/master/src/clj_basic_algorithms/number_generation

# Communication

Feel free to ask questions and propose things in Issues.

# License

This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or distribute
this software, either in source code form or as a compiled binary, for any
purpose, commercial or non-commercial, and by any means.

See the LICENSE file for more details.
