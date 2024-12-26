Each bit follows the following pattern:

```
x00 > XOR(y00) > z00
    > AND(y00) > hmc > XOR(brk) > z01
                     > AND(brk) > pjk
x01 > XOR(y01) > brk ------^
    > AND(y01) > vpj > OR(pjk) > rhd > XOR(rcm) > z02
                                     > AND(rcm) > qvh
x02 > XOR(y02) > rcm ----------------------^
    > AND(y02) > wwc > OR(qvh) > sgb > XOR(psb) > z03
                                     > AND(psb) > tjj
…
```

While fiddiling a round,
I also found a solution when swapping `jmh,nqq,sqr,z18`,
which seems to be an accident.
Later I found out that I need to swap 4 *pairs*,
so the result has to contain 8 wires.
