a = 0  # <- input; 0 or 1
f = 10551345 if a else 945
a = 0
for i in range(1, f + 1):
    if f % i == 0:
        a += i

print("Result:", a)
