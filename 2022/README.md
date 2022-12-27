# Advent of Code 2022

## Summary

I generally enjoyed this year's AoC.
There were about 3 to 4 tasks I struggled with,
sometimes because I was too stubborn with my initial implementation idea
and sometimes because I didn't read the task properly
or forgot an important detail while working on it.
You can read more details
in the [individual task notes][#individual-days] below.

Also this was the first time I actually completed all tasks
and I only needed 2 extra days.

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

I tried to consider performance in the initial implementation
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
(sometimes up to 10)
and you should be able to deduce most of these
from the individual descriptions below.
Other than that,
I needed about 0.5-3 hours for each day,
including refactoring and discussions.

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

### [Day 01]

Nothing exciting here.

### [Day 02]

Classic example of the tests needing more lines than the code.

### [Day 03]

I used a `u64` as a bit set
since we only need 26\*2 fields.
This makes the code much faster than with a `HashSet`.

### [Day 04]

Nothing exciting here.

### [Day 05]

I struggled a bit with making the same code
reusable across the two parts without much duplication
and ended up having having to duplicate an entire line
because I couldn't get Rust to treat a reversed iterator
like any other.
Dyn trait objects really aren't cool to work with.

### [Day 06]

The "challenge" here is to write a solution
that completes in `O(n*m)` time,
where `m` is the window size.

### [Day 07]

The first task with a more complex data structure.
My first implementation used a reference to the current directory item,
which required wrapping everything in `Rc<RefCell<_>>`.
I rewrote it later to remember the path to the current item
instead of a reference to the item
so that I could acquire it only when needed.

### [Day 08]

Don't know what to say about this,
other than that I first went with a sub-optimal approach
that I ended up tweaking later.

### [Day 09]

Nothing exciting here.

### [Day 10]

One trick you can use here is to simply collect the outputs
and allow different instructions to provide different amounts of outputs.
With that, the solution becomes trivial.
(This was inspired by someone else's solution in Haskell.)

### [Day 11]

Part 1 consisted mostly of parsing the input
and then simply implementing what was written in the instructions.

Part 2 required some knowledge in discrete math
because all the tests test for whether the accumulator
can be divided by some number,
meaning it's equal to 0 in a modulo room
(I don't know the proper term in English
and wasn't able to find it either).
The only operations on the accumulator are multiplication and addition,
which both wrap around on a modulo room,
meaning you can easily reduce the accumulator
by taking the remainder of it
when divided by the product of all divisor numbers
(which coincidentally are all prime numbers
but is not relevant here).

### [Day 12]

The first path-finding task.
I implemented a simple BFS algorithm
and re-used it for part 2
by adjusting the branching code
and exiting on the first potential target.


### [Day 13]

I liked this day because there were to possible solutions here.
The first is to properly parse the input into a recursive data structure
and then implement comparison as described.
The second is to optimize the comparison to operate on the strings directly
while utilizing pre-processing
to reduce multi-digit numbers (only `10`) to a single character.
I decided to go for the former
because I could overload the comparison operators here
to work nicely with various built-in functions
like sorting a vector,
making the core logic of both parts trivial.

### [Day 14]

A basic but fun task.
One could either simulate each step from the top
or use backtracking from the previous position
to save some cycles
especially for the longer falling blocks.
The biggest speedup here
is to store the map inside a 2-dimensional `Vec`
instead of using a `HashMap` or set.


### [Day 15]

This sounds complex at first but really isn't,
as long as you use a simple shortcut
by considering each scanner & beacon pair
as a circle with a radius
(based on Manhattan distance).
There are some smarter and faster ways to go about part 2
than the simple brute-force method with shortcuts
that I implemented,
but they are also much more complex to read and write,
so I considered this to be a rather elegant solution.

### [Day 16]

Path-finding on crack.
Compared to the previous task,
this one had an exploding number of nodes that could be visited,
making it infeasible to compute
without reducing the problem space first.
The first things to implement are to strip out
nodes where no pressure can be released
and to build a directed graph with the remaining nodes.
Then implement one of the many path-finding algorithms,
ideally one with a cost function like Dijkstra,
and find the solution with the most.
Part 2 is basically the same
when you use both operators as a node key
and advance both at the same time.
My solution here is actually generic over N operators.

### [Day 17]

That was a nasty one that cost me quite some time to get right.
It was obvious that there would be cycles for part 2
that needed to be found,
but reliably finding them for any input
turned out to be pretty hard.
I'm content with my solution,
but there is probably a better way to go about it.
I'm just glad my idea to speed this up worked.

### [Day 18]

Fairly straight-forward
and a welcome breather after the past 2 days.

### [Day 19]

I tried very hard not to implement a brute-force solution
– perhaps a bit too hard –
only to realize after 8 hours or so
that I wasn't gonna get it to work.
After that, I resorted to brute-forcing with a reduced problem space
by filtering out states that would be useless
or are strictly worse than others.
I ended up making two errors in these optimizations after a long day
and revisited the task later
to find these errors rather quickly.

Part 2 was basically free afterwards.

### [Day 20]

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
only to later (on the next day and thanks to a tip) realize
that I forgot to consider duplicate numbers
when determining which number to move next.

Because this and the previous day too me so long,
I ended up deferring working on day 21 to the following day
and skipped 22.
I would end up working on that last.

### [Day 21]

This was one of the most fun tasks.

### [Day 22]

I worked on this task as the last one,
not because I found it difficult in particular
but because I needed a break
and hardly had time on that day.

Part 2 was annoying
and I quickly decided to go for a solution with hard-coded transitions
instead of a generic solution for any possible net.
I ended up making some errors in the initial map,
so I used a trick I found somewhere else:
Moving 200 steps in any direction from any point
should always wrap around to the starting position.

### [Day 23]

"Reading comprehension" is the name of this day's task.
If you read everything correctly
and didn't forget it half-way through,
this was a simple task
and primarily required translating the description into code.

### [Day 24]

Basically standard path-finding.
I implemented a BFS solution
since that is easy to implement
and performant enough for this task.
The only notable thing was that I didn't transform
the blizzard map for each iteration.
Instead, for each direction
I tracked back the number of steps
a blizzard would have to make to land
at a certain position after n steps
and looked it up in a 2-dimensional `Vec`.

### [Day 25]

This was a short but fun task,
reusing concepts of generic number-to-string conversions with an arbitrary base
but adding a twist on top.


[Day 01]: https://adventofcode.com/2022/day/1
[Day 02]: https://adventofcode.com/2022/day/2
[Day 03]: https://adventofcode.com/2022/day/3
[Day 04]: https://adventofcode.com/2022/day/4
[Day 05]: https://adventofcode.com/2022/day/5
[Day 06]: https://adventofcode.com/2022/day/6
[Day 07]: https://adventofcode.com/2022/day/7
[Day 08]: https://adventofcode.com/2022/day/8
[Day 09]: https://adventofcode.com/2022/day/9
[Day 10]: https://adventofcode.com/2022/day/10
[Day 11]: https://adventofcode.com/2022/day/11
[Day 12]: https://adventofcode.com/2022/day/12
[Day 13]: https://adventofcode.com/2022/day/13
[Day 14]: https://adventofcode.com/2022/day/14
[Day 15]: https://adventofcode.com/2022/day/15
[Day 16]: https://adventofcode.com/2022/day/16
[Day 17]: https://adventofcode.com/2022/day/17
[Day 18]: https://adventofcode.com/2022/day/18
[Day 19]: https://adventofcode.com/2022/day/19
[Day 20]: https://adventofcode.com/2022/day/20
[Day 21]: https://adventofcode.com/2022/day/21
[Day 22]: https://adventofcode.com/2022/day/22
[Day 23]: https://adventofcode.com/2022/day/23
[Day 24]: https://adventofcode.com/2022/day/24
[Day 25]: https://adventofcode.com/2022/day/25
