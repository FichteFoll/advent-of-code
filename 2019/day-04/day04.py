from collections import Counter

passwords1 = 0
passwords2 = 0

for num in range(147981, 691423 + 1):
    digits = list(str(num))
    if sorted(digits) == digits:
        ctr = Counter(digits)
        if len(ctr) < len(digits):
            passwords1 += 1
        if 2 in ctr.values():
            passwords2 += 1

print("Part 1: {}".format(passwords1))
print("Part 2: {}".format(passwords2))
