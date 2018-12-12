const N = 50000000000
input = readlines("input.txt")

str_to_char_arr(s) = split(s, "") |> a -> map(a) do s convert(Char, s[1]) end

shortstate(s::Vector{Char}, n::Int) = shortstate(join(s, ""),n )

shortstate(s::String, n::Int) = begin
    arr::Vector{Int} = []
    for (i, c) in enumerate(s)
        if c == '#'
            push!(arr, i + n)
        end
    end
    return arr
end

function main(initialstate::String, rules::Vector{String})
    m = 5
    state = [fill('.', m); str_to_char_arr(initialstate); fill('.', m)]
    n = -1 * m

    i = 1
    while i <= N
        state2 = evolveState(state, rules)
        ss1 = shortstate(state, n)
        ss2 = shortstate(state2, n)
        ssd = ss2[1] - ss1[1]
        state = state2
        if all(i -> ssd == ss2[i] - ss1[i], 1:length(ss2))
            n += (N - i) * ssd
            println("Found pattern at $i w/ diff $ssd")
            break
            return
        end
        
        state = [fill('.', m); state; fill('.', m)]
        n -= m
        i += 1
    end

    sum::Int = 0
    for (i, c) in enumerate(state)
        if c == '#'
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