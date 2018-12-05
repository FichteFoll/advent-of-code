def main():
    a = 1
    b = 57
    c = b
    h = 0
    if a:
        b *= 100
        b -= 100_000
        c = b
        c -= 17_000
    while True:
        f = 1
        d = 2
        while True:
            e = 2
            while True:
                g = d
                g *= e
                g -= b
                if not g:
                    f = 0
                e += 1
                g = e
                g -= b
                if not g:
                    break
            d += 1
            g = d
            g -= b
            if not g:
                break
        if not f:
            h += 1
        g = b
        g -= c
        if not g:
            return
        b += 17
