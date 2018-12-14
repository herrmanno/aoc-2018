const input = "509671" # 92510 509671
const N = map(i -> parse(Int, i), split(input, ""))
const N_LENGTH = length(N)

mutable struct Node
    val::Int
    next::Union{Node,Nothing}
    prev::Union{Node,Nothing}
end

function Node(val::Int)::Node
    node = Node(val, nothing, nothing)
    node.next = node
    node.prev = node
    return node
end

function Base.show(io::IO, node::Node)
    print(io, "$(node.val), ")
end

function append(node::Node, next::Node)::Node
    next.prev = node
    next.next = node.next

    node.next.prev = next
    node.next = next

    return next
end

function compare_backwards(node::Node, vals::Vector{Int})::Bool
    for i in 0:(length(vals) - 1)
        if node.val != vals[end - i]
            return false
        else
            node = node.prev
        end
    end
    return true
end

function main()
    firstNode = Node(3)
    lastNode = Node(7)
    n = 2
    elf1 = firstNode
    elf2 = lastNode

    append(elf1, elf2)

    while true
        r = elf1.val + elf2.val
        if r > 9
            lastNode = append(lastNode, Node(convert(Int, floor(r / 10))))
            if compare_backwards(lastNode.prev, N)
                break
            end
            n += 1
        end
        lastNode = append(lastNode, Node(r % 10))
        if compare_backwards(lastNode.prev, N)
            break
        end
        n += 1
        
        for i in 1:(1 + elf1.val)
            elf1 = elf1.next
        end

        for i in 1:(1 + elf2.val)
            elf2 = elf2.next
        end
    end

    println(n - length(N))

    node = firstNode
    # while node != lastNode
    #     print(node)
    #     node = node.next
    # end
end

main()
