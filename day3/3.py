import re
from collections import defaultdict

with open("3.txt", "r") as file:
    items = file.read().rstrip().split('\n')


f = open("3.txt").read().strip()
lines = f.split('\n')
mat = [[c for c in line] for line in lines]
R = len(mat)
C = len(mat[0])

p1 = 0
nums = {}
for r in range(len(mat)):
  gears = set() 
  n = 0
  has_part = False
  for c in range(len(mat[r])+1):
    if c<C and mat[r][c].isdigit():
      n = n*10+int(mat[r][c])
      for rr in [-1,0,1]:
        for cc in [-1,0,1]:
          if 0<=r+rr<R and 0<=c+cc<C:
            ch = mat[r+rr][c+cc]
            if not ch.isdigit() and ch != '.':
              has_part = True
            if ch=='*':
              gears.add((r+rr, c+cc))
    elif n>0:
      for gear in gears:
            if gear not in nums:
                nums[gear] = [n]
            else:
                nums[gear].append(n)
      if has_part:
        p1 += n
      n = 0
      has_part = False
      gears = set()

print(p1)

print(nums)
p2 = 0
for k,v in nums.items():
  if len(v)==2:
    p2 += v[0]*v[1]
print(p2)