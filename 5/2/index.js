const input = require("fs").readFileSync("input.txt").toString()

function reduce(input) {
				let i = 0
				while (i < input.length) {
					let c1 = input[i]
					let c2 = input[i + 1]

					if (c1 && c2 && Math.abs(c1.charCodeAt(0) - c2.charCodeAt(0)) === 32) {
								input.splice(i, 2)
								i -= 1
					} else {
						i++
					}
				}
				return input.length
}

let results = new Array(26).fill(0).map((_, idx) => {
				let newInput = input.replace(new RegExp(String.fromCharCode(idx + 65), "ig"), "")
				return { idx, length: reduce(newInput.split("") }
})

results.sort((a, b) => a.length - b.length)

console.log(`The shortes possible polymer length is ${results[0].length}`)
