a = 66245665

b = a % 8
c = a // 2**(b ^ 7)
b = b ^ c
a = a // 8
out = b % 8

print(f"{a, b, c, out}")
