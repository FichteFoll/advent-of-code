def main():
    a = 1
    h = 0
    b = c = 57
    if a:
        b = b * 100 - 100_000
        c = b - 17_000

    while True:
        f = 1
        d = 2
        while True:
            e = 2
            while True:
                if d * e == b:
                    f = 0
                e += 1
                if e == b:
                    break
            d += 1
            if d == b:
                break
        if not f:
            h += 1
        if b == c:
            return h
        b += 17
