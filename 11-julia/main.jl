function neighbours(x::CartesianIndex, y::Matrix{Int64})
  indices = CartesianIndices(y)
  max(first(indices), x-oneunit(x)):min(last(indices), x+oneunit(x))
end

function glow(x::Matrix{Int64}, n::Int64)
  glows = findall(x -> x >= 9, x)
  if isempty(glows)
    (x, n)
  else
    x[glows] .= -1
    for i in Iterators.flatten(map(a -> neighbours(a, x), glows))
      if x[i] >= 0
        x[i] += 1
      end
    end
    glow(x, n+length(glows))
  end
end

function next(x::Matrix{Int64})
  (x, n) = glow(copy(x), 0)
  (x .+ 1, n)
end

function calc(x::Matrix{Int64})
  z = copy(x)
  numFlashes = 0
  allFlashed = 0
  for i in 1:1000
    (z,m) = next(z)
    if i <= 100
      numFlashes += m
    end
    if m == 100
      allFlashed = i
      break
    end
  end
  return (numFlashes, allFlashed)
end

if length(ARGS) < 1
  println("Usage: ", Base.basename(Base.source_path()), " <filename>")
  exit(1)
end
input = reshape(map(x -> parse(Int,x), collect(filter(isdigit, read(ARGS[1], String)))), (10,10))
(part1, part2) = calc(input)
println("Part 1: ",part1)
println("Part 2: ",part2)
