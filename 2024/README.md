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
