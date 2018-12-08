input = split(readstring("./input.txt"), " ")

next() = parse(Int, shift!(input))

struct Node
  children::Vector{Node}
  meta::Vector{Int}
end

function value(node::Node)::Int
  if isempty(node.children)
    sum(node.meta)
  else
    [value(node.children[m]) for m in node.meta if m in indices(node.children, 1)] |> sum
  end
end

function consume()::Node
  childCount = next()
  metaCount = next()
  Node(
    [consume() for i in 1:childCount],
    [next() for i in 1:metaCount]
  )
end

rootValue = value(consume())

println("The value of root is $rootValue")
