using Test

const MAX_SIZE = 300

function main(gridSerialID::Int)
    m = Matrix{Int}(undef, MAX_SIZE, MAX_SIZE)
    for y in 1:MAX_SIZE
        for x in 1:MAX_SIZE
            m[y,x] = calculatePower(x, y, gridSerialID)
        end
    end

    coords = nothing
    t = 0
    for y in 1:MAX_SIZE
        for x in 1:MAX_SIZE
            for s in 1:(MAX_SIZE - x + 1)
                t2 = squareTotalPower(m, x, y, s)
                if t2 > t
                    t = t2
                    coords = (x, y, s)
                end
            end
        end
    end

    return coords
end

function calculatePower(x::Int, y::Int, gridSerialID::Int)
    rackID = x + 10
    power = rackID * y
    power += gridSerialID
    power *= rackID
    powerString = "$power"
    power = (length(powerString) > 2 ? powerString[end - 2] : "0") |> s -> parse(Int, s)
    power -= 5
    return power
end

function squareTotalPower(m::Matrix{Int}, x::Int, y::Int, size::Int)
    sum = 0
    for i in y:min(MAX_SIZE, (y + size - 1))
        for j in x:min(MAX_SIZE, (x + size - 1))
            sum += m[i, j]
        end
    end
    return sum
end

main(parse(Int, readline("input.txt"))) |> println