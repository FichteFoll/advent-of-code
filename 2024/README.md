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

TODO
