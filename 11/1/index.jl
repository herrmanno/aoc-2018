using Test

function main(gridSerialID::Int)
    m = Matrix{Int}(undef, 300, 300)
    for y in 1:300
        for x in 1:300
            m[y,x] = calculatePower(x, y, gridSerialID)
        end
    end

    coords = nothing
    t = 0
    for y in 1:298
        for x in 1:298
            t2 = squareTotalPower(m, x, y)
            if t2 > t
                t = t2
                coords = (x, y)
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

function squareTotalPower(m::Matrix{Int}, x::Int, y::Int, size = 3)
    sum = 0
    for i in y:(y+2)
        for j in x:(x+2)
            sum += m[i, j]
        end
    end
    return sum
end


if "TEST" in keys(Base.ENV)
    @test calculatePower(122, 79, 57) == -5
    @test calculatePower(217, 196, 39) == 0
    @test calculatePower(101, 153, 71) == 4

    @test main(18) == (33, 45)
end

main(parse(Int, readline("input.txt"))) |> println