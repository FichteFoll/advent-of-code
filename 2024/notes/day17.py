a = 66245665
prog = [2,4,1,7,7,5,1,7,4,6,0,3,5,5,3,0]


def comp():
    global a
    while a != 0:
        b = a % 8
        c = a // 2**(b ^ 7)
        b = b ^ c
        a = a // 8
        yield b % 8


print(",".join(map(str, comp())))
# expected: 1,4,6,1,6,4,3,0,3
