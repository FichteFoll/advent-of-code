# Advent of Code 2025

## Summary

To be added later.


## Procedure

Each day I followed the following three steps:

1. Read and solve part 1.
1. Read and solve part 2
   (trying to reuse as much of part 1 as possible).
1. (Optional) Refactor and compare with other solutions (via Discord)
   to see what else could be improved.

I tried to consider performance in the initial implementation
and that mostly held true,
avoiding brute-force solutions where possible
and achieving competitive benchmark results for the most part.
There are of course exceptions to this.

Note that I follow the philosophy of
only parsing the string input
into a useful data structure for further processing
according to the task description.
This has to be a 1-to-1 translation
where useless data may be omitted
but outside of that the operation must be reversible.

As such,
something like sorting is not permitted
but translating CPU instructions into an enum is.


## Project Structure & Compilation

### Haskell

On Arch Linux, I used the following packages:

- [`ghc`](https://www.haskell.org/ghc/) & `ghc-libs`: standard Haskell compiler and interpreter & libraries
- [`haskell-linear`](https://hackage.haskell.org/package/linear): vector library
- [`haskell-split`](https://hackage.haskell.org/package/split): utilities for splitting lists (like `String`s)

Optional:

- [`haskell-language-server`](https://github.com/haskell/haskell-language-server): self-explanatory
- [`haskell-hspec`](https://hspec.github.io/): testing framework
- [`haskell-criterion`](http://www.serpentine.com/criterion/): benchmarking framework

For convenience, a `justfile` is included in the `haskell` subfolder:

```bash
$ cd haskell
$ just -l
Available recipes:
    bench DAY # Run benchmarks for DAY with input
    default
    run DAY   # Run script for DAY with input
    test DAY  # Run tests for DAY
$ just run 01
…
```

---

To run the solutions manually,
navigate to the respective folder
and use `runhaskell` to run files or tests.
For the `Main.hs` file, pipe in the input file:

```bash
cd haskell/day01
runhaskell MainSpec.hs
runhaskell Main.hs < ../../inputs/day01.txt
```

To run the benchmarks,
compile the accompanying benchmarking code
and simply run it.

```bash
cd haskell/day01
ghc -dynamic -O2 -main-is Bench Bench.hs
./Bench < ../../input/day01.txt
```


## Individual Days

The following may contain spoilers.

### Day 01

TODO
