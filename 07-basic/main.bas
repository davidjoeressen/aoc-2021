'' quicksort from https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#FreeBASIC
sub quicksort(a() as integer, first as integer, last as integer)
  dim length as integer = last - first + 1
  if length < 2 then return
  dim pivot as integer = a(first + length\2)
  dim lft as integer = first
  dim rgt as integer = last
  while lft <= rgt
    while a(lft) < pivot
      lft += 1
    wend
    while a(rgt) > pivot
      rgt -= 1
    wend
    if lft <= rgt then
      swap a(lft), a(rgt)
      lft += 1
      rgt -= 1
    end if
  wend
  quicksort(a(), first, rgt)
  quicksort(a(), lft, last)
end sub

function part1fuel(dat() as integer, n as integer, hp as integer) as integer
  dim fuel as integer = 0
  for i as integer = 1 to n
    fuel += abs(dat(i)-hp)
  next i
  return fuel
end function

function part2fuel(dat() as integer, n as integer, hp as integer) as integer
  dim fuel as integer = 0
  for i as integer = 1 to n
    dim dist as integer = abs(dat(i)-hp)
    fuel += dist*(dist+1)/2
  next i
  return fuel
end function

if command(1) = "" then
  print "Usage: "; command(0); " <file>"
  end
end if

'' read input
open command(1) for input as #1
dim dat(1000) as integer
dim n as integer

while not eof(1)
  n += 1
  input #1, dat(n)
wend
close #1

''sort
quicksort(dat(), 1, n)

'' calculate fuel for part 1
print "Part 1:"; part1fuel(dat(), n, dat(n\2))

'' calculate fuel for part 2
dim hp as integer = 0
'' start with mean as hp
for i as integer = 1 to n
  hp += dat(i)
next i
hp = hp / n

dim fuel as integer = part2fuel(dat(), n, hp)
do
  dim nf as integer = part2fuel(dat(), n, hp + 1)
  if nf < fuel then
    hp += 1
    fuel = nf
  else
    exit do
  end if
loop
do
  dim nf as integer = part2fuel(dat(), n, hp - 1)
  if nf < fuel then
    hp -= 1
    fuel = nf
  else
    exit do
  end if
loop
print "Part 2:";fuel
