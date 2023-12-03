# Advent of Code 2023

## Summary

to be added later

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

## Project Structure & Compilation

### Haskell

On Arch Linux, I used the following packages:

- [`ghc`](https://www.haskell.org/ghc/) & `ghc-libs`: standard Haskell compiler and interpreter & libraries
- `haskell-linear`: vector library

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

### Rust

Nothing yet

## Individual Days

The following may contain spoilers.

### Day 01

Took way longer for this than I'd like to admit,
but I have reasons …
This day's delays were sponsored by:

- setting up my environment (haven't used Haskell in years)
- trying to (statically) compile the code instead of just "running" it
- using the wrong variable for testing part 2
- using the wrong function for testing part 2
- why the fuck are words allowed to overlap, Eric?!

Not the usual simple task for the first day,
but I'm still mad about the last point.


### Day 02

More straight forward compared to the previous day,
but parsing text like that in Haskell is not too pleasant.
Still, I think I found a decent solution – powered by list comprehensions.
