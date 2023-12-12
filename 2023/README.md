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


### Day 03

Somehow, getting the condition right for the second part
took me a couple of tries (and a forced break).
After I was finished,
I simplified it using more vector features and lenses,
but that turned out to need 30 times as much time,
so I discarded it.


### Day 04

Finally something simple that Haskell works well for.


### Day 05

The most complex input so far,
but parsing is manageable if you omit the unimportant details.
The order of the numbers in the input is very unintuitive,
so I quickly changed that during parsing.
Afterwards it's rather straigh-forward for part 1.

Part 2 was brute-forceable for my first implementation
and took about 15 minutes
(with optimizations! otherwise it would hog too many resources).
However, there are some ways to optimize this
and I did that on the following day when I had more time.
Hint: just handle multiple numbers at once.


### Day 06

The task here was pretty simple
and implementing it was, too.
Even more surprising that the bruteforce method
solved part 2 in less than a second.

If you want it to be even faster,
you can also solve the quadratic formula instead,
which I did later as well,
since I was curious about implementing it in Haskell.
Turns out that converting numeric types
is still not my forte.


### Day 07

Sounded a bit rough at first,
bit was actually pretty fun to implement.
Two edge cases bit me hard in part 2,
but I'm pretty satisfied with the result.


### Day 08

Part one was fairly straight forward.
Part two had me questioning at various times
whether my mental model for detecting the loops was correct,
after I had already found out that brute-forcing is not an option,
until I just implemented it to see if it works.
I eventually made some assumptions on the input
(guarded by an `assert`)
to simplify the implementation,
which makes the example not pass
but it matters not.


### Day 09

Nothing noteworthy.
Simple day for Haskell.


### Day 10

Skipped for now due to lack of time.


### Day 11

Takes a little bit of mental gymnastics
but otherwise pretty straight forward
(especially when you have a `transpose` function).
I liked adapting my solution for part 2,
even though I had a nasty off-by-one bug at first.
