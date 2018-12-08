input = split(readstring("./input.txt"), " ")

next() = parse(Int, shift!(input))

struct Node
  children::Vector{Node}
  meta::Vector{Int}
end

function reduce(f, node::Node, init = 0)
  init = f(init, node)
  for c = node.children
    init = reduce(f, c, init)
  end
  return init
end

function consume()::Node
  childCount = next()
  metaCount = next()
  
  return Node(
    [consume() for i in range(1, childCount)],
    [next() for i in range(1, metaCount)],
  )
end

root = consume()
total = reduce((acc, node) -> begin
  return acc + sum(node.meta)
end, root::Node, 0::Int) 


println("The sum of all meta nodes is $total")