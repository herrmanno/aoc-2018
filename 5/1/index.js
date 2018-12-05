const input = require("fs").readFileSync("input.txt").toString().split("")
// const input = "dabAcCaCBAcCcaDA".split("")

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

console.log(`The resulting polymer has a length of ${input.length} units`)
