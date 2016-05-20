# HLinear

HLinear is a Haskell implementation of the PLE decomposition of matrices over division rings.
It writes an arbitrary matrix as a product of a permutation matrix, a lower triangular matrix with diagonal entries 1, and an echelon matrix.

Features:

*
*


## Installation

1. Install [stack](https://haskellstack.org).
2. Install [git](https://git-scm.com).
3. Install [flint](https://flintlib.org).
4. Create a working directory for HLinear and its dependencies:
```
mkdir hlinear-all; cd hlinear-all
```
5. Get [natural-test](https://github.com/martinra/natural-test):
```
git clone https://github.com/martinra/natural-test.git
```
6. Get [vector-test](https://github.com/martinra/vector-test):
```
git clone https://github.com/martinra/vector-test.git
```
7. Get [algebraic-structures](https://github.com/martinra/algebraic-structures):
```
git clone https://github.com/martinra/algebraic-structures.git
```
8. Get [hflint](https://github.com/martinra/hflint):
```
git clone https://github.com/martinra/hflint.git
```
9. Get HLinear:
```
git clone https://github.com/martinra/hlinear.git
```
10. Build HLinear:
```
cd hlinear
stack build
```
11. Test HLinear:
```
stack test
```
The log file with the test results can be found at .stack-work/logs/hlinear-0.0.1-test.log

12. Run a simple benchmark:
```
stack bench
```
The log file with the benchmark results can be found at .stack-work/logs/hlinear-0.0.1-bench.log

## How to use

This is just a sketchy illustration of how to interact with HLinear from Haskell.
For details and code you can immediately start adapting to your needs, see [HLinear-example](https://github.com/aghitza/hlinear-example).

```
(...various imports...)

main :: IO ()
main = do
  let m = M.fromListsUnsafe [[1,2,3],[4,5,6]] :: Matrix FMPQ
      hk = D.unPLEDecomposition $ FU.pleDecompositionFoldUnfold m
      e = Hk.echelonForm hk
      em = EF.toMatrix e
  putStrLn "echelon form:"
  print em
```


## License

GPLv3, see [LICENSE](LICENSE).
