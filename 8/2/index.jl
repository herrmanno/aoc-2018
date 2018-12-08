input = split(readstring("./input.txt"), " ")

next() = parse(Int, shift!(input))

struct Node
  children::Vector{Node}
  meta::Vector{Int}
end

function value(node::Node)::Int
  return if (length(node.children) == 0)
    sum(node.meta)
  else
    map(node.meta) do meta
      if meta in indices(node.children, 1)
        value(node.children[meta])
      else
        0
      end
    end |> sum
  end
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
rootValue = value(root)

println("The value of root is $rootValue")