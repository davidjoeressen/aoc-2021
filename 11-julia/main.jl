function getNeighbours(x::CartesianIndex, y::Matrix{Int64})
  indices = CartesianIndices(y)
  max(first(indices), x-oneunit(x)):min(last(indices), x+oneunit(x))
end

function next(x::Matrix{Int64})
  n = 0
  while (glows = findall(a -> a >= 9, x); !isempty(glows))
    x[glows] .= -1
    for g in glows
      neighbours = filter(a -> x[a] >= 0, getNeighbours(g, x))
      x[neighbours] .+= 1
    end
    n += length(glows)
  end
  (x .+ 1, n)
end

function calc(x::Matrix{Int64})
  z = copy(x)
  numFlashes = 0
  allFlashed = 1
  while ((z,m) = next(z); m != length(z))
    if allFlashed <= 100
      numFlashes += m
    end
    allFlashed += 1
  end
  return (numFlashes, allFlashed)
end

parse_input(input::String) = reshape(map(x -> parse(Int, x), collect(filter(isdigit, input))), (10, 10))

if length(ARGS) < 1
  println("Usage: ", Base.basename(Base.source_path()), " <filename>")
  exit(1)
end
(part1, part2) = read(ARGS[1], String) |> parse_input |> calc
println("Part 1: ", part1)
println("Part 2: ", part2)
