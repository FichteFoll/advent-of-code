a, b, c, d, f = 1, 0, 0, 0, 0  # start
f += 2
f *= f
f *= 19
f *= 11
d += 4
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

b = 1
while b <= f:
    c = 1
    while c <= f:
        d = b * c
        if d == f:
            a += b
        c += 1
    b += 1
