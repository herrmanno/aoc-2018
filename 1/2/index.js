let input = require("fs").readFileSync("input.txt").toString()
let arr = input.split("\n")
let acc = {}
let n = 0
let j = 0
while (true) {
    n = eval(`${n}${arr[j]}`)
    j = (j + 1) % arr.length
    if (!acc[n]) {
        acc[n] = true
    } else {
        break
    }
}

console.log(`The first number to appear twice is ${n}`)
