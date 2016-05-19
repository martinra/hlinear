# HLinear

HLinear is a Haskell implementation of the PLE decomposition of matrices over division rings.
It writes an arbitrary matrix as a product of a permutation matrix, a lower triangular matrix with diagonal entries 1, and an echelon matrix.

Features:

*
*


## Installation

1. Install [stack](https://haskellstack.org).
1. Install [git](https://git-scm.com).
1. Install [flint](https://flintlib.org).
1. Create a working directory for HLinear and its dependencies:
```
mkdir hlinear-all
```
1. Get natural-test:
```
git clone https://github.com/martinra/natural-test.git
```
1. Get vector-test:
```
git clone https://github.com/martinra/vector-test.git
```
1. Get algebraic-structures:
```
git clone https://github.com/martinra/algebraic-structures.git
```
1. Get hflint:
```
git clone https://github.com/martinra/hflint.git
```
1. Get hlinear:
```
git clone https://github.com/martinra/hflint.git
```
1. Build hlinear:
```
cd hlinear
stack build
```
1. Test hlinear:
```
stack test
```

## How to use

```
stack ghci
> import HFlint.FMPQ
> import HLinear.Matrix as M
> import HLinear.PLE.FoldUnfold as FU
> import HLinear.PLE.Decomposition as D
> import HLinear.PLE.Hook as Hk
> let m = M.fromListsUnsafe [[1, 2, 3], [4, 5, 6]] :: Matrix FMPQ
> let hk = D.unPLEDecomposition $ FU.pleDecompositionFoldUnfold m
> Hk.toMatrices hk
```


## License

GPLv3, see [LICENSE](LICENSE).
