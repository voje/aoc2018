#!/usr/bin/python

# wrong answer ...

import math

def match(a, b):
    matches = (abs(ord(a) - ord(b)) == 32)
    # print("{} {} {}".format(a, b, matches))
    return matches

def compact(txt):
    i = 0
    while i < (len(txt)-1):
        if match(txt[i], txt[i+1]):
            txt = txt[:i] + txt[i+2:]
        else:
            i += 1
    return txt

if __name__ == "__main__":

    with open("input_0.txt") as f:
        txt = f.read()

    change = True
    while change:
        l = len(txt)
        txt = compact(txt)   
        change = (len(txt) != l)

    print(len(txt))
    print (txt)
