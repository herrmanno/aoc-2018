let input = require("fs").readFileSync("input.txt").toString().split("\n")

let arr = new Array(1000 * 1000).fill(0).map(() => [0, undefined])

for (let line of input) {
	let [_, id, x, y, w, h] = line.match(/#(\d)+ @ (\d+),(\d+): (\d+)x(\d+)/).map(Number)
	for (let i = x; i < x + w; i++) {
		for (let j = y; j < y + h; j++) {
			let [ val ] = arr[1000 * j + i]
			arr[1000 * j + i] = [val + 1, id]
		}
	}
}

const ids = new Set(arr.filter(el => el[0] === 1).map(el => el[1]))
console.dir([...ids])
// console.log(`There are ${arr.filter(n => n > 1).length} squares of fabric, that are used more than once`)
