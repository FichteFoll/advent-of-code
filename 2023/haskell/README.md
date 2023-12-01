# Advent of Code 2023

## Haskell

### Setup

For Arch, I used the following packages:

- [`ghc`](https://www.haskell.org/ghc/) & `ghc-libs`: standard Haskell compiler and interpreter & libraries
- [`haskell-language-server`](https://github.com/haskell/haskell-language-server): self-explanatory
- [`haskell-hspec`](https://hspec.github.io/): testing framework

### Environment

Navigate to a folder and use `runhaskell` to run files or tests.
For the `Main.hs` file, pipe in the input file:

```bash
cd haskell/day01
runhaskell MainSpec.hs
runhaskell Main.hs < ../../inputs/day01.txt
```


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
