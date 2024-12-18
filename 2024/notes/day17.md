## Input
```
Register A: 66245665
Register B: 0
Register C: 0

Program: 2,4,1,7,7,5,1,7,4,6,0,3,5,5,3,0
```

## Translated

BST A
BXL 7
CDV B
BXL 7
BXC \_
ADV 3
OUT B
JNZ 0

## As Code

ADV 3
OUT B

```py
while a != 0:
    b = a % 8
    b = b ^ 7
    c = a // 2**b
    b = b ^ 7
    b = b ^ c
    a = a // 2**3
    print(b % 8)
```

## Simplified

```py
while a != 0:
    b = a % 8
    c = a // 2**(b ^ 7)
    b = b ^ c
    a = a // 8
    print(b % 8)
```

## Reversed

`b` and `c` are not carried over between loops,
so we can ignore them for the starting point.

The original `a` must not be `0`
to not violate the loop condition.

Since I was *way* too lazy to figure out the math
behind computing the original `b`
from the given output and the resulting `a`,
The following code prints out each valid original `b`
for a given `a` and `out`:

```py
for b_ in range(8):
    a_ = a * 8 + b_
    c = a_ // 2**(b_ ^ 7)
    out_ = (b_ ^ c) % 8
    if out == out_ and a_ != 0:
        print(a_, out_)
```

I then use this in a recursive DFS
to determine the smallest valid most original `a`,
iterating backwards over the program input.

The full code is in `day17_reverse.py`.
