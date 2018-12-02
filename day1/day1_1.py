#!/usr/bin/python3

init = []

with open("input_1.txt") as f:
    init = [int(line) for line in f]

print(init)
newlist = [sum(init[:(i+1)]) for i,e in enumerate(init)]
print(newlist)

def cycle(lst):
    while True:
        for el in lst:
            yield el

my_cycle = cycle(init)
"""
for i in range(20):
    print(my_cycle.next())
"""

while True:
    new = my_cycle.next() + newlist[-1]
    # print(new)
    if new in newlist:
        print(new)
        break
    newlist.append(new)


