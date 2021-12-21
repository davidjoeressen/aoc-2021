import os
import std/sequtils

type
  Image = tuple
    main: seq[seq[int]]
    rest: int
  Index = tuple
    x: int
    y: int

func getOrDefault(image: Image, x: int, y: int): int =
  if x in 0..image.main.high:
    let row = image.main[x]
    if y in 0..row.high:
      return row[y]
  return image.rest

const neighbours: seq[Index] =
  @[(-1,-1),(-1,0),(-1,1),
    (0,-1),(0,0),(0,1),
    (1,-1),(1,0),(1,1)]

func getNext(image: Image, alg: seq[int], x: int, y: int): int =
  let i = neighbours.map(proc(i: Index): int = image.getOrDefault(x + i.x, y + i.y)).foldl(a shl 1 + b)
  alg[i]

proc enhance(image: Image, alg: seq[int]): Image =
  let width = image.main[0].len
  let height = image.main.len
  let newmain = toSeq(-1..height).map(
    proc(x: int): seq[int] =
      toSeq(-1..width).map(
        proc(y: int): int = image.getNext(alg, x, y)))
  let newrest =
    if image.rest == 0: alg[0]
    else: alg[^1]
  return (newmain, newrest)

func parse(c: char): int =
  if c == '#': 1 else: 0

func countBright(image: Image): int =
    image.main.map(proc(row: seq[int]): int = row.foldl(a + b, 0)).foldl(a + b, 0)

proc main() =
  if commandLineParams().len < 1:
    echo "Usage: ", getAppFilename(), " <filename>"
    return
  let input = toSeq(lines(commandLineParams()[0]))
  let alg = input[0].map(parse)
  let main = input[2..input.high].map(proc(s: string): seq[int] = s.map(parse))
  let image = (main, 0)
  var result = image
  for i in 1..50:
    result = result.enhance(alg)
    if i == 2: echo "Part 1: ", result.countBright
  echo "Part 2: ", result.countBright

main()
