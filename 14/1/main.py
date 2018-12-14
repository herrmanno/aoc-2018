from math import floor

N = 509671

elf1 = 0
elf2 = 1
recipes = [3,7]

while len(recipes) < N + 10:
    r = recipes[elf1] + recipes[elf2]
    recipes += [int(floor(r / 10))] if floor(r / 10) > 0 else []
    recipes += [r % 10]
    elf1 = (elf1 + 1 + recipes[elf1]) % len(recipes)
    elf2 = (elf2 + 1 + recipes[elf2]) % len(recipes)

print("".join([str(i) for i in recipes[N:N+11]]))
