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


### Day 12

I took the pattern matching approach here,
which worked pretty well for part 1 I'd say.
Unfortunately, it obviously doesn't perform well on part 2,
so I derived another solution using memoization in a Map,
but that still didn't really bring the speed improvements I expected
(and that I need for this to complete in less than a couple hours).
Because I've had a problem with optimizing tasks like this in the past
(i.e. the last AoC I attempted in Haskell only),
which eventually lead me to burn out of it,

I'm skipping finalizing part 2 until some time later,
hopefully.


### Day 13

Powered by list comprehensions and arrows,
this day was actually pretty fun.
Fortunately, the "brute-force" method here performs just fine.
Note like I have any idea currently how to do it differently.


## Day 14

This was fun at first until part 2 came around and I …

- implemented a cool constant-space repetition detection,
  only to realized that the cycle repetitions are not adjacent;
- got confused by the fixity of `` `mod` `` for a bit;
- assumed the rotation went clockwise instead of counter-clockwise
  because I can't read (or remember easy stuff);
- had to implement grid-printing code to find the above;
- explicitly read the docs to check
  whether `iterate` returns the initial value as the first item,
  only to then understand the opposite by missing 2 characters;
- had fortunately just one off-by-one error that wasn't too hard to debug;
- and probably did more things.

All in all, this is definitely within the AoC spirit,
but I struggled more with it than I should have.

Eventually, someone else pointed out to me
how I could very easily operate directly on the input
and I couldn't resist implementing it.
For my original solution,
refer to the file history.


# Day 15

This task was written in a very imperative way,
which is generally not what Haskell fits well with,
but it turned out to be rather fun after all
when using lenses.
A pro Haskeller can probably do it smarter,
but I think my solution here is decent.


# Day 16

Again, a rather iterative task
that needs to be solved with recursion in Haskell,
but it was still somewhat fun to translate into code.

For part two,
the simple brute-force method is fast enough for me
and I'm not convinced there is a smarter one anyway.


# Day 17

Skipped due to lack of time.


# Day 18

Part 1 allowed for some decently creative ideas (imho),
one of which I eventually implemented,
knowing fully well that it won't scale into multiple orders of magnitude.
Of course, part 2 just had to be exactly that
and I haven't really thought of an entirely different solution.
I've heard that there is a trick for solving this efficiently,
but am unsure if I will be able to figure it out myself.

Skipping part 2 for now.


# Day 19

Parsing was more work work than implementing part 1,
but part 2 gave this a little twist.
First of all,
I had to change my parsing
since it was too specialized for part 1,
but additionally it started to feel a lot like day 5
with added data complexity,
which wasn't looking too fun in Haskell.
Fortunately, I have grown somewhat accustomed to lenses by now,
so the end result wasn't too horrible,
but I procrastinated on this task for 2 days.
(Additionally, my PC broke down in the middle of it,
which killed some of my motivation
and several hours of my time for hardware debugging.)


# Day 20

This is the day I stopped working on AoC for this year.
I'm writing this one year later
and I can see that I implemented part 1
but only managed to do a bruteforce implementation of part 2
that does not finish in a reasonable period of time.

Afterwards I didn't find the time for AoC anymore
and consequently also skipped the remaining days.
