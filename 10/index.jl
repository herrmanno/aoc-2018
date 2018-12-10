const MAX_X = 280
const MAX_Y = 200
const input = readlines("input.txt")

struct Velocity
    x::Int
    y::Int
end

mutable struct Light
    x::Int
    y::Int
    velocity::Velocity
    
    function Light(s::String)
        x, y, vx, vy = map(match(r"(-?\d+).*?(-?\d+).*?(-?\d+).*?(-?\d+)", s).captures) do i
            parse(Int, i)
        end
            
        new(x, y, Velocity(vx, vy))
    end
end

function move(light::Light)
    light.x += light.velocity.x
    light.y += light.velocity.y
end

function formtext(lights::Array{Light})
    xmin = min(map(l -> l.x, lights)...)
    xmax = max(map(l -> l.x, lights)...)
    ymin = min(map(l -> l.y, lights)...)
    ymax = max(map(l -> l.y, lights)...)

    return (
        !any(l -> l.x > MAX_X || l.y > MAX_Y, lights) &&
        xmax - xmin < 150 &&
        ymax - ymin < 100 &&
        length(filter(l -> l.x == xmin, lights)) > 3 &&
        length(filter(l -> l.x == xmax, lights)) > 3
    )
end    

function main()
    lights =  map(Light, input)
    matrix = Matrix{String}(undef, MAX_Y, MAX_X)

    n = 0
    while true
        map(move, lights)
        if formtext(lights)
            println("About to print ($n)")
            readline(stdin)
            

            for i in 1:MAX_Y
                for j in 1:MAX_X
                    matrix[i, j] = " "
                end
            end

            for l in lights
                matrix[l.y, l.x] = "#"
            end
            
            for i in 1:MAX_Y
                for j in 1:MAX_X
                    print(matrix[i, j])
                end
                print("\n")
            end
        end
        n += 1
    end

end

main()