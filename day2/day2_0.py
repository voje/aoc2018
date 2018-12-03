import string

with open("input_0.txt") as f:
    # get rid of newlines
    indata = [line[:-1] for line in f]    

print(indata[:10])

twos = 0
threes = 0

for instring in indata:
    tw = False
    thr = False
    for letter in string.ascii_lowercase:
        c = instring.count(letter)
        if c == 2 and not tw:
            twos += 1
            tw = True
        elif c == 3 and not thr:
            threes += 1
            thr = True

print("twos: ", twos)
print("threes: ", threes)
print("mul: ", twos * threes)

