prog = [2,4,1,7,7,5,1,7,4,6,0,3,5,5,3,0]


def rev_step(prog, a):
    if not prog:
        yield a
        return
    *prog_, out = prog
    for b_ in range(8):
        a_ = a * 8 + b_
        c = a_ // 2**(b_ ^ 7)
        out_ = (b_ ^ c) % 8
        if out == out_ and a_ != 0:
            yield from rev_step(prog_, a_)


print(min(rev_step(prog, 0)))
# expected: 265061364597659
