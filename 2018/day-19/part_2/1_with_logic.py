a, b, c, d, f = 1, 0, 0, 0, 0  # start
f += 2
f *= f
f *= 19
f *= 11
d += 4  # 0+4 at first iteration
d *= 22
d += 21
f += d

if a:
    d = 27
    d *= 28
    d += 29
    d *= 30
    d *= 14
    d *= 32
    f += d
    a = 0

# <- line 21
b = 1
c = 1
d = b * c
if d == f:  # => d; 4
    a += b
c += 1  # 8
if c > f:  # => d; 9
    b += 1
    if b > f:  # => d; 14
        return
    else:
        goto 23  # => c = 1
else:
    goto 24  # => d = b * c
