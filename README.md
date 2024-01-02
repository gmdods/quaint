# quaint
A(nother) C++ units/quantities library

Experiment: Trying to implement a good syntax for adding units.

## Inspirations

- [Unitful](https://github.com/PainterQubits/Unitful.jl)
- [mp-units](https://github.com/mpusz/mp-units) (See also Youtube!)
- [au](https://github.com/aurora-opensource/au) (See also Youtube!)

## Template Metaprogramming (TMP)

For complicated metaprogramming, implement first in Haskell (see `template.hs`).
For cons-list, we use the `type_sequence` idiom to wrap template packs.

