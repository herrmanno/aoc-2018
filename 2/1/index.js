let input = require("fs").readFileSync("input.txt").toString().split("\n")
let n2s = 0
let n3s = 0

input.forEach(input => {
				let isort = [...input].sort().join("")
				if (isort.match(/(.)\1\1/)) n3s++
				if (isort.match(/(.)\1/)) n2s++
})

console.log(`The checksum is ${n3s * n2s}`)
