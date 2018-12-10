const N_PLAYERS = 473
const N_MARBLES = 70904 * 100
const PRINT = "PRINT" in keys(ENV)

mutable struct Marble
  value::Int64
  next::Union{Nothing,Marble}
  prev::Union{Nothing,Marble}
end
Marble(val::Int64) = Marble(val, nothing, nothing)

function append(marble::Marble, next::Marble)
  oldnext = marble.next

  marble.next = next
  next.prev = marble
  next.next = oldnext
  oldnext.prev = next

  return next
end

function remove(marble::Marble)
  marble.next.prev = marble.prev
  marble.prev.next = marble.next
  return marble.next
end

score = Dict{Int,Int}()
player = 2
marble = Marble(0, nothing, nothing)
marble.next = marble
marble.prev = marble

for n = 1:N_MARBLES
  global marble
  if n % 23 != 0
    marble = append(marble.next, Marble(n))
  else
    for i = 1:7
      marble = marble.prev
    end

    score[player] = get(score, player, 0) + n
    score[player] += marble.value
    marble = remove(marble)
  end

  if PRINT
    m2 = marble
    while m2.value != 0
      m2 = m2.prev
    end
    print("[$player] ")
    for _ = 0:n
      if m2 == marble
        print("($(m2.value)) ")
      else
        print("$(m2.value) ")
      end
      m2 = m2.next
    end
    print("\n")
  end

  global player = 1 + player % N_PLAYERS
end

println(max(values(score)...))