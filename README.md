# skiplist-sandbox

This was intended as a learning space to toy around with implementing my own persistent data structure in a language where there is no high-quality implementation to give me direction. Earlier in freshman year, I wrote a skiplist in C, and wanted to try in Haskell. It was a great exercise in recursion and problem solving, as well as benchmarking and measuring performance in a lazy language.


## Background on the data structure
A SkipList can be seen as stacked layers of linked lists, where each list element also points to its identical representation below ([see these images on wikipedia](https://en.wikipedia.org/wiki/Skip_list#Description)). For any given element in a layer of the structure, there is a one half chance (decided randomly), that it will be "promoted" to the layer above. So each layer of the SkipList "skips" over half of the elements on the layer below it, on average. This gives average time complexity O(logn) for all operations, and average space complexity O(2n).

## Implementation
Of course, randomness doesn't play perfectly with pure Haskell, so the SkipList itself holds a random number generator as a field in the structure. It currently uses the `StdGen` type from `System.Random`, but any instance of the `RandomGen` typeclass can be supplied and will work. In my implementation I prioritized simplicity over performance, and so I ended up with an implementation with a speed somewhere between a linked list and `Data.Map`. I'm sure it could be optimized heavily. The SkipList was also made a member of several typeclasses like `Functor`, `Foldable`, and `NFData` (for benchmarking).

## Modules and packaging
The SkipList interface is exported by Data.SkipList.Pure. There also exists an internal module Data.SkipList.Pure.Internal with the actual implementation of the structure.

There is a benchmarking suite using the [Criterion library](https://hackage.haskell.org/package/criterion) that compares the SkipList to the canonical Data.Map on a few operations. The benchmarks can be made and run with `stack bench`.

There are also a few tests in a test suite using Tasty and HUnit that can be made and run with `stack test`.
