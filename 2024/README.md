# Advent of Code 2024

## Summary

This years puzzles had quite an interesting difficulty curve,
starting of with harder tasks on weekdays
and easier ones on the weekend
where it used to be the other way around in the past years
and which also made more sense.
Even in the later days,
any given day could be a tough one
with the surrounding ones being comparatively easy.
Overall, there were a couple tricky ones,
many involving path-finding
and a few

On the weekend of the 20th I ran out of time
for the first day
and did not find any time for the second either,
so I skipped that entirely,
but I got back on track afterwards.
After solving the 25th I continued working on the tasks I skipped,
but those (un-)fortunately turned out to be
probably the most tricky ones this year
and after spending a few hours on one of them,
I decided to abandon the rest of it
because after almost 26 days
I figured I could use a longer break.
Besides that,
I also had some other things planned for the holidays
with higher priority.


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
like an N-dimensional coordinate structure.

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
I started with an iterative approach
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
Unfortunately, my enum choice made the code rather unwieldy
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

### Day 14

First part was comparatively trivial
and a known puzzle from previous years,
mostly subject to off-by-one errors (of which I had one).

The second part jumps you
with a lack of information
and requires some creativity
in formulating what you *think* the result could look like
into code.
I had a good hunch here
but missed the part in the description
that mentions "most" robots being part of the image …

### Day 15

The first part instructions sound simple
and allow some freedom in solving it.

The second part are an interesting twist on this
and depending on how you solved part 1,
you can re-use some parts of your code,
but the logic becomes more complex.
Theoretically, the algorithm of part 1 is a full subset of part 2
but can be much simpler because it is linear.
For performance reasons,
I decided to re-use of my half of my part 1 solution in part 2
because the horizontal movements remain basically the same
and are optimized for my storage (using `Vec::swap`).
Overall, this day needed quite a lot of code
compared to the previous ones,
but it's not exactly hard.

## Day 16

The first path-finding task of the year.
I shot myself in the foot multiple times this day.
First by switching to a `BTreeMap` for part 2
when I should have remained with a set
because otherwise the part would be de-duplicated,
which is exactly what you don't want for part 2.
Second by forgetting half of Dijkstra
and not implementing a cache
of the lowest cost to a certain point so far
to abandon paths that are already longer.
The latter took me way too much time to remember/figure out.

# Day 17

This was a decently tough day.
Not the first part,
as that was a pretty simple custom instruction set unit,
but for the second one
it fist took me a short while
to first implement it in brute-force code,
only to then realize
that this will likely not compute
in the foreseeable future.

Fortunately, the actual instructions were fairly simple
and we've had much worse tasks like this in the past,
so I solved part 2 through reverse engineering the code,
some *tiny little* brute force,
and Python,
because I am much faster prototyping there.
The solution is most likely *not* applicable for each input
since I analyzed this specifically for mine,
but it's not like this matters for such a task.
It is very easy
to make tiny errors during this process, though.

# Day 18

The second path-finding task,
this time utilizing A\*
to (hopefully) skip some unnecessary steps.
I made an annoying off-by-one error
when specifying/respecting the grid size,
which caused my otherwise-correct implementation
to never find a solution.

Part 2 is just brute-force,
but it only runs for 1.4 seconds
and that is fine in my book for day 18.

# Day 19

The task sounds simple at first,
but in isolation it's kind of interesting.
However, the problem is basically
a combination of several patterns we've had so far,
so it's pretty much just copying those.
I tried to think
about some non-obvious ways to implement it
and to golf a bit,
but there isn't that much to optimize here.

My part 1 solution is not optimal
because it is a BFS without a solution cache,
but I think it presents a neat contrast
to part 2's DFS.

# Day 20

Yet another path-finding task
with a small twist to make it more interesting.
The twist *is* interesting,
albeit horribly worded
and easy to misunderstand,
but I guess most complex problems in software
eventually result in path-finding.
The wording caused me to re-implement several parts
and I needed several debugging sessions
and an external tip
to eventually solve part 2.
After that, it was comparatively easy
but I just didn't think of it.

# Day 21

Skipped due to time constraints.

# Day 22

Kind of a fun task
for getting to know `itertools` better.
Fortunately it was also relatively easy to debug,
because I missed that the example for part 2
is not the same as for part 1.

# Day 23

Quite a standard graph problem,
but not something I have had to implement until now,
specifically part 2.
Had to fiddle a bit with the implementation
and it takes 1.3 seconds to complete,
but that sounds good enough for a day 23 task.

# Day 24

I started this after the 25th.
The first part is fairly straight forward,
while the second is more of a puzzle.
I made some assumptions about the machine form
(i.e. that it uses a consistent pattern for each output)
that I did not want to verify individually,
but unfortunately my initial attempt at an otherwise automated solution failed.

~~I have not yet decided whether I will continue working on it
because I'm experiencing some burnout after 25 days.~~
I will not continue working on this.
I still have an idea I could implement,
but I don't understand yet why my first attempt doesn't solve it.
I could also solve it manually,
but that seems really cumbersome and unfun,
so I'd rather not solve it at all.

# Day 25

The majority of this task is parsing,
so it was comparatively boring,
but I suppose it works as a fade-out.

At the time of writing,
I still need to do day 21 and 24,
because I simply skipped them due to lack of time.
I will do those next.
