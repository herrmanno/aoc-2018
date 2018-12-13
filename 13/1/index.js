let input = require("fs").readFileSync(__dirname + "/input.txt").toString().split("\n")

const isCart = s => ["<", ">", "v", "^"].includes(s)

const STATE = ["LEFT", "STRAIGHT", "RIGHT"]
const DIRECTION = ["LEFT", "UP", "RIGHT", "DOWN"]


function main() {
    /** @type {[{ x:number, y: number, direction: number, state: number }]} */
    const carts = []

    input.forEach((line, y) => {
        [...line].forEach((char, x) => {
            if (isCart(char)) {
                const direction = "<^>v".indexOf(char)
                carts.push({ x, y, direction, state: 0 })
                input[y][x] = direction % 2 ? "|" : "-"
            }
        })
    })

    while (true) {
        carts.sort((a, b) => {
            const crit1 = a.y - b.y
            return crit1 ? crit1 : a.x - b.x
        })

        for (const cart of carts) {
            const collideCoord = moveCart(cart)
            if (collideCoord) {
                console.dir(collideCoord)
                return                
            }
        }

        if ("PRINT" in process.env) {
            input.forEach((line, y) => {
                const str = [...line].map((char, x) => {
                    const c = carts.find(c => c.x == x && c.y == y)
                    if (c) {
                        return "<^>v"[c.direction]
                    } else {
                        return char
                    }
                }).join("")
                console.log(str)
            })
            
            console.log()
        }
    }

    function moveCart(cart) {
        const nextPos = nextCartPos(cart)
        const nextChar = input[nextPos.y][nextPos.x]

        if (nextChar === "+") {
            if (cart.state === 0 /* turn LEFT */) {
                cart.direction = (cart.direction + DIRECTION.length - 1) % DIRECTION.length
            } else if (cart.state === 2 /* turn RIGHT */) {
                cart.direction = (cart.direction + 1) % DIRECTION.length
            }
            cart.state = (cart.state + 1) % STATE.length
        } else if (nextChar === "/") {
            if (cart.direction % 2 === 1 /* UP | DOWN */) {
                // turn RIGHT
                cart.direction = (cart.direction + 1) % DIRECTION.length
            } else {
                // turn LEFT
                cart.direction = (cart.direction + DIRECTION.length - 1) % DIRECTION.length
            }
        } else if (nextChar === "\\") {
            if (cart.direction % 2 === 1 /* UP | DOWN */) {
                // turn LEFT
                cart.direction = (cart.direction + DIRECTION.length - 1) % DIRECTION.length
            } else {
                // turn RIGHT
                cart.direction = (cart.direction + 1) % DIRECTION.length
            }        
        } else if (!nextChar || nextChar === "") {
            throw new Error("Cart has left the lane")
        }

        if (carts.find(c => c.x === nextPos.x && c.y === nextPos.y)) {
            return nextPos
        } else {
            cart.x = nextPos.x
            cart.y = nextPos.y
        }

    }
}

function nextCartPos(cart) {
    const { x, y, direction } = cart
    switch (direction) {
        case 0 /* LEFT */:  return { x: x - 1, y }
        case 1 /* UP */:    return { x, y: y - 1 }
        case 2 /* RIGHT */: return { x: x + 1, y }
        case 3 /* DOWN */:  return { x, y: y + 1 }
        default: throw new Error(`invalid direction '${direction}'`)
    }
}

main()