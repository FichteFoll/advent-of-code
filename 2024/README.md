# Advent of Code 2024

## Summary

to be added later (hopefully)


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

### Rust

Each day is written as a separate binary
while sharing some common code
like macros
to reduce boilerplate
and some other useful utilities
like an N-dimentional coordinate structure.

You can run them via
```sh
# Grabs the respective day's input from the `input` folder.
$ cargo run --bin day01
# Can also take the input file name from an env var.
INPUT=custom_input.txt cargo run --bin day08
```

Each day also has tests to ensure they are working
(and to make developing things at a time more convenient),
usually with the example inputs
as well as my benchmarks for my actual inputs
that I store in the `input` folder
to check their performance.
Some tests take a bit longer to run in debug builds.
The same `--bin` argument as for running can be used
with `cargo test` and `cargo bench` respectively.


## Individual Days

The following may contain spoilers.

### Day 01

As usual, the first day is a simple task
that usually revolves around parsing numbers
and then doing some operations on it.
It also provided a clear reminder
for why `itertools` is such a useful crate
and can shorten your code quite a bit.

### Day 02

This day was special
because I added the additional challenge
of using the `helix` TUI editor
and no mouse
instead of my usual Sublime Text setup.

It did not take much time to get working,
but I did notice I was significantly slower than usual,
especially because of the mouse thing.
Now, it's not that `helix` doesn't support mouse inputs,
but I specifically wanted to learn the keyboard interactions.

Regarding the actual task,
I started with an iterational approach
which worked fine for part 1
but ran into problems for part 2,
because you need to do some form of backtracking
and cannot simply remove the first non-matching number in a series.
I ended up going through several iterations and refactorings,
because I kept running into edge cases or unclean code
(also in part due to editor unfamiliarity).
Thanks again, `itertools`.

### Day 03

The task description immediately screams "regular expressions"
and even though I initially intended to avoid simply using those,
thinking about how to implement the necessary algorithm
got me so very close to a regex implementation
that I might as well simply re-use an existing one.

My part 2 solution has a small gotcha with the input
because I use a `.` to match all the garbage
between `don't()` and `do()` instructions
but the input is spread over several lines,
so I needed to account for that.

### Day 04

The first coordinate task already on the fourth day?
Well, here we go.

For part 1,
I implemented a breath-first search
because I was thinking about utilizing lazy evaluation,
but Rust isn't Haskell,
so I had to collect each step into a vector.
This resulted in a lot of memory being allocated unnecessarily
and made the solution quite slow.

Part 2 was not compatible with my part 1 solution at all,
so I wrote something entirely different –
this time with DFS,
`u8::checked_sub`,
the `?` operator,
and a good ol' coordinate switcheroo (x vs y axis)
that also took me some time to spot.

### Day 05

I had a pesky logic error
in my head
and did not read correctly
that rules only need to be considered
if both numbers involved
are part of the sequence to test.

The code for part 2 isn't exactly optimized,
but I wanted to get it done fast
so I can work on other stuff.
May revisit later.

### Day 06

This task was actually quite fun –
assuming you didn't have to reinvent the grid handling code
like I did by re-using the `Grid2D` struct I built for 2021 already.

Had a small gotcha with the instructions
by not considering
that I would need to turn more than once
if the path after turning was also blocked.

Overall, this has been my favorite day so far.

### Day 07

For some reason,
the first weekend task since the first day
is much easier than the previous days.
Not much to say here
other than that I brute-forced
used recursion
and a simple early abort.

### Day 08

Another grid task
where my existing grid code came in handy.
The necessary algorithms aren't fancy,
but getting the types right
as I unified both parts by utilizing a closure parameter
proved to be a bit challenging
compared to the rest of this task.

### Day 09

Weekday tasks are definitely the harder ones.
However, this one is a bit different from the others
because it actually has a practical application
and visualizes the problems of fragmentation.
I considered using a `Vec` for each cell
but then figured I could be smarter by treating files like blocks.
Unfortunately, my enum choice made the code rather unwieldly
and required lots of pattern matching.
I'd like to optimize that
but probably won't find the time.

I also had to write a manual test case here
to find a bug in my implementation for part 2.

### Day 10

The first path-finding task,
but we're used from that from all the previous years.
After solving part 1 and 2 independently,
I unified them (again) using closures
just like for day 8.
I'm undecided whether this makes it more readable
but I'm closer to a generic solution now.

### Day 11

Trivial part 1
and memoization is the name for part 2.
I'm glad I'm not using Haskell this time
because I've had my fair share of trouble there.

### Day 12

This was a tricky but fun task
and took me the longest so far,
I think.
The solution for part 1
is decently intuitive
but for part 2
you have to think a bit around the *corner*.
I'm really thankful for the test cases provided here
because they helped a lot with identifying and ironing out
*edge* cases.

### Day 13

The obligatory linear algebra task has arrived.
The task description of part 1
was very obvious about
brute force not being the solution for part 2,
so I went with the mathematical solution for the first part already.
After a small hiccup with my formula
that took a long time to debug,
because I refused to write this out on paper
(or properly in latex),
and another small hiccup
based on incorrect assumptions about what `checked_div` means
and whether or not negative numbers can occur,
this turned out to be a rather neat task.
