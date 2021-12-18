#!/usr/bin/env python
import sys
import re

def add(v1,v2):
    return (v1[0]+v2[0],v1[1]+v2[1])

def minxvel(x1):
    x = 0
    while x*(x+1)/2 < x1:
        x += 1
    return x

def hit(velocity, target):
    pos = (0,0)
    vel = velocity
    xrange = range(target[0],target[1]+1)
    yrange = range(target[2],target[3]+1)
    maxy = 0
    while pos[0] <= target[1] and pos[1] >= target[2]:
        if pos[1] > maxy:
            maxy = pos[1]
        if pos[0] in xrange and pos[1] in yrange:
            return maxy
        pos = add(pos,vel)
        vel = (max(0,vel[0]-1),vel[1]-1)
    return -1

def solve(input):
    target = re.findall('-?[0-9]+',input)
    x1 = int(target[0])
    x2 = int(target[1])
    y1 = int(target[2])
    y2 = int(target[3])
    maxy = 0
    count = 0
    for x in range(minxvel(x1),x2+1):
        for y in range(y1,abs(y1)):
            my = hit((x,y),(x1,x2,y1,y2))
            if my > maxy:
                maxy = my
            if my >= 0:
                count += 1
    print(f"Part 1: {maxy}")
    print(f"Part 2: {count}")

def main():
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <file>")
        exit(1)
    file = open(sys.argv[1],'r')
    input = file.readline()
    file.close()
    solve(input)

if __name__ == '__main__':
    main()
