input = readlines("input.txt")

str_to_char_arr(s) = split(s, "") |> a -> map(a) do s convert(Char, s[1]) end

function main(initialstate::String, rules::Vector{String})
    m = 5
    state = [fill('.', m); str_to_char_arr("$initialstate"); fill('.', m)]
    n = -1 * m

    for i in 1:20
        state = evolveState(state, rules)
        state = [fill('.', m); state; fill('.', m)]
        n -= m
        # println("""$i: $(join(state, ""))""")
    end

    sum::Int = 0
    for (i, c) in enumerate(state)
        if c == '#'
            println([(i - 1), ((i - 1) + n)])
            sum += (i - 1) + n
        end
    end
    
    println(sum)
end

function evolveState(state::Vector{Char}, rules::Vector{String})::Vector{Char}
    nextState = fill('.', length(state))
    for i in 3:(length(state)-2)
        slice = state[(i-2):(i+2)]
        nextState[i] = evolvePlant(slice, rules)
    end
    return nextState
end

function evolvePlant(slice::Vector{Char}, rules::Vector{String})::Char
    for rule in rules
        if all(i -> rule[i] == slice[i], 1:5)
            return rule[end]
        end
    end
    return '.'
end

main(
    split(input[1], ":")[2] |> strip |> s -> convert(String, s),
    input[3:end]
)