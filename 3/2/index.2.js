let input = require("fs").readFileSync("input.txt").toString().split("\n")

let claims = input.map(line => line.match(/#\d+ @ (\d+),(\d+): (\d+)x(\d+)/).map(Number).slice(1))
let ids = new Array(claims.length).fill(true)

for (let i = 0; i < claims.length; i++) {
	let c1 = claims[i]
	for (let j = 0; j < i; j++) {
		let c2 = claims[j]
		if (overlap(c1, c2) || overlap(c2, c1)) {
			ids[j] = false
			ids[i] = false
		}

	}
}

let [_, idx] = ids.map((val, idx) => [val, idx]).filter(([val]) => val)[0]
console.log(`The patch #${idx+1} does not overlap with any other patch`)

/**********************************************************/
//					Utilities
/**********************************************************/

function overlap([x1, y1, w1, h1], [x2, y2, w2, h2]) {
	return (
		x1 < (x2 + w2) && (x1 + w1) > x2 &&
		y1 < (y2 + h2) && (y1 + h1) > y2
	)
}
