def main():
    a = 1
    h = 0
    b = c = 57
    if a:
        b = b * 100 - 100_000  # -94300
        c = b + 17_000  # -77300

    # technically b could exceed c, but not semantically
    for b in range(b, c + 17, 17):
        for d in range(2, b):
            if b % d == 0 and b / d >= 2:
                break
        else:
            h += 1
    return h


if __name__ == '__main__':
    print(main())
