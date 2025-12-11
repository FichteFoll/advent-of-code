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

This was a fun task for the first day,
allowing me to struggle with what I remember from Haskell
from two years ago
while simultaneously supporting a simple bruteforce solution
and a "smart" solution for part 2
that can really get your gears grinding.

### Day 02

Another bruteforceable task
that provided several avenues for solving,
albeit working on a string was probably easiest.
I then spent an unreasonable amount of time
trying to create a point-free version for my initial list comprehension.

### Day 03

Took me a bit longer
than I would have expected
to translate the algorithm I had in my head into code,
but ultimately this wasn't exactly a difficult task
once you understood the instructions.
In my initial solution,
I didn't think of leaving enough characters for the next recursion
to definitely reach a solution,
which is why I used `Maybe` a lot.
I refactored the code later to make use of this trick.

### Day 07

Once you got the logic down,
it's mostly folding through the lines.
`IntSet` and `IntMap` yield speedups of 5-10×.

### Day 08

This was a neat task
where I mostly had to fight with Haskell types,
not knowing the functions whose features I wanted,
and going down a Lenses rabbit hole,
even though I ended up not actually using any.
I do feel like we're rather tame on the difficulty still,
but I expect one of the following days to be a filter day.

### Day 09

This was was tricky.
Part 1 was basically free,
but part 2 had me thinking in circles,
misunderstanding (and forgetting) the task description
and overcomplicating the soluction a lot,
until I eventually found a performant solution
with the help of someone else.
Definitely the hardest problem so far.
That's what I get for jinxing.

## Day 11

Every year has a task with graph traversal
and this year is no different.
Although all these problems are similar in nature,
I still enjoy implementing path-finding algorithms every now and then –
especially so if it's not always in the same language –
and compared to the 2D grids,
graphs puzzles are usually more interesting.

Also, I mis-typed "svr" as "srv" twice
because who the fuck uses "svr"?
