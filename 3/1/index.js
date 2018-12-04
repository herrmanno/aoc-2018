let input = require("fs").readFileSync("input.txt").toString().split("\n")

let arr = new Array(1000 * 1000).fill(0)

for (let line of input) {
				let [_, x, y, w, h] = line.match(/#\d+ @ (\d+),(\d+): (\d+)x(\d+)/).map(Number)
				for (let i = x; i < x + w; i++) {
								for (let j = y; j < y + h; j++) {
												arr[1000 * j + i]++
								}
				}
}

console.log(`There are ${arr.filter(n => n > 1).length} squares of fabric, that are used more than once`)
