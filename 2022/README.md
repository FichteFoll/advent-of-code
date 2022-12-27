# Advent of Code 2022

## Summary

I generally enjoyed this year's AoC.
There were about 3 to 4 tasks I struggled with,
sometimes because I was too stubborn with my initial implementation idea
and sometimes because I didn't read the task properly
or forgot an important detail while working on it.
You can read more details
in the [individual task notes][#individual-days] below.

Rust was a good choice.
For once, it allowed me to get to use this language again
because it's not relevant at work
and not in most of my private projects or scripts either
(those are primarily in Python)
and because I had several other people I knew
also working with Rust,
meaning it was a good opportunity
to exchange knowledge, tips and whatnot.
Just in general a good point of discussion
if you don't have to also discuss
ow to build a set-like intersection with a map
because your standard library doesn't implement sets.

## Procedure

Each day I followed the following three steps:

1. Read and solve part 1.
1. Read and solve part 2
   (trying to reuse as much of part 1 as possible).
1. (Optional) Refactor and compare with other solutions (via Discord)
   to see what else could be improved.

I tried to consider perfomance in the initial implementation
and that mostly held true,
avoiding brute-force solutions where possible
and achieving competitive benchmark results for the most part.
There are of course exceptions to this.

## Stats

My stats are recorded on the website are fairly useless
because I didn't start working on a day at a regular time
and took breaks frequently due to appointments or just eating.
I did some manual time tracking for some days
where I needed more than 4 hours to complete them
and you should be able to deduce most of these
from the individual descriptions below.
Other than that, I needed about

## Takeaways for Rust

- Compared to the last year,
  `let-else` has been stabilized and I tried to use it quite frequently.
  I've also used it in the last year when it was still unstable,
  but this is was definitely a very good addition.

- Print debugging is still very useful and I love the `dbg!` macro.
  Its output is sometimes a bit verbose,
  but if that's bothers you
  there is always the option to use `println!` instead.

- I keep wanting to `.flat_map` an `Option`
  but the appropriate function is called `.and_then` instead.

- The error message(s) for trying to `.flatten()` an `Option<&Option<_>>`
  could use some improvements.
  rustc would complain about the type not implementing the `Iterator` trait
  when I specifically wanted to call `.flatten()` on the outer `Option`
  to acquire the inner option.
  The workaround is to invoke `.cloned()` first,
  but that took a while for me to grasp from the error message.

- I tried using `LazyCell` (unstable) once
  instead of falling back to `lazy_static` like I used to in previous years.
  Unfortunately, the restriction of
  the wrapped type needing to implement `Sync`
  is quite annoying in practice
  and makes the type signature rather complex.
  This is doubly annoying because anything `static` needs type annotations
  and cannot infer the type automatically,
  so you end up writing the name of wrapper structs twice.
  Either that or I didn't understand how to use this feature properly.

## Dependencies

- **itertools**:
  Provides some generally useful utility methods for iterators.
  Additionally, I like the `izip!` and `iproduct!` macros
  for reducing nesting levels and long-ish `Iterator` call chains.

- **paste**:
  Used for my test macros and reducing boilerplate.

- **test-case**:
  Some days had examples with different inputs,
  making this the ideal candidate
  for reducing test code duplication.
  Requires the `custom_test_frameworks` feature
  but was definitely worth it.

- **fnv** (optional):
  Just a faster hashing algorithm than the one in `std`.
  The dependency is optional and abstracted away
  by using `aoc2022::collections::{HashSet, HashMap}`.
  Benchmarks without it can be run
  by passing `--no-default-features` to the cargo command.

## Individual Days

The following may contain spoilers.

## Day 01

Nothing exciting here.

## Day 02

Classic example of the tests needing more lines than the code.

## Day 03

I used a `u64` as a bit set
since we only need 26\*2 fields.
This makes the code much faster than with a `HashSet`.

## Day 04

Nothing exciting here.

## Day 05

I struggled a bit with making the same code
reusable across the two parts without much duplication
and ended up having having to duplicate an entire line
because I couldn't get Rust to treat a reversed iterator
like any other.
Dyn trait objects really aren't cool to work with.

TODO

### Day 19

I tried very hard not to implement a bruteforce solution
– perhaps a bit too hard –
only to realize after 8 hours or so
that I wasn't gonna get it to work.
After that, I resorted to bruteforcing with a reduced problem space
by filtering out states that would be useless
or are strictly worse than others.
I ended up making two errors in these optimizations after a long day
and revisited the task later
to find these errors rather quickly.

Part 2 was basically free afterwards.

### Day 20

For part 1 I chose to use a `LinkedList`
since I haven't used that in quite a while.
The `Cursor` and `CursorMut` structs are unstable
and have some rough edges when working with them,
especially when wrapping around at either end of the list,
but the Task is generally achievable with them.
A speed-up is possible by splitting the list,
inserting an item at the split position
and then joining them again
so that I didn't have to move the cursor back to my initial position
to proceed with the next number,
but that would make the code more complex for edge cases
where wrapping is required.

Since the solution of part 1 was not reusable for part 2,
I had to rewrite the entire thing
where I chose to use a `Vec` that I would slice-copy
to shift entries around.
This was also significantly faster for part 1.
However, the omission of duplicate numbers in the examples got me good
where I spent roughly 4 hours trying to find an error in my shifting code,
rewriting it two times in the process,
only to later (on the next day and thanks to a tip) realite
that I forgot to consider duplicate numbers
when determining which number to move next.

Because this and the previous day too me so long,
I ended up deferring working on day 21 to the following day
and skipped 22.
I would end up working on that last.

### Day 22

I worked on this task as the last one,
not because I found it difficult in particular
but because I needed a break
and hardly had time on that day.

Part 2 was annoying
and I quickly decided to go for a solution with hardcoded transitions.
I ended up making some errors in the initial map,
so I used a trick I found somewhere else:
Moving 200 steps in any direction from any point
should always wrap around to the starting position.

### Day 24

Basically standard pathfinding.
I implemented a BFS solution
since that is easy to implement
and performant enough for this task.
The only notable thing was that I didn't transform
the blizzard map for each interation.
Instead, for each direction
I tracked back the number of steps
a blizzard would have to make to land
at a certain position after n steps
and looked it up in a 2-dimensional `Vec`.

### Day 25

This was a short but fun task,
reusing concepts of generic number-to-string conversions with an arbitrary base
but adding a twist on top.
