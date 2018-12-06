const input = require("fs").readFileSync("./input.txt").toString().split("\n").map(line => line.split(",").map(Number))

const maxX = Math.max(...input.map(coord => coord[0]))
const maxY = Math.max(...input.map(coord => coord[1]))

const arr = new Array(maxX * maxY).fill(null)

for (let i = 0; i < arr.length; i++) {
  const x0 = i % maxX
  const y0 = (i - x0) / maxX
  const minDistance = input.reduce((min, [x,y]) => {
    return Math.min(min, Math.abs(x0 - x) + Math.abs(y0 - y))
  }, Infinity)

  const minPoints = input.map(([x,y], idx) => ({x,y, idx})).filter(({x, y}) => {
    return minDistance === Math.abs(x0 - x) + Math.abs(y0 - y)
  })

  if (minPoints.length > 1) {
    arr[i] = null
  } else if (minPoints.length === 1) {
    arr[i] = minPoints[0].idx + 1
  } else {
    throw new Error("Illegal state")
  }
}

const grouped = arr.reduce((acc, cell) => {
  if (!cell || cell == -1) {
    return acc
  } else {
    return { ...acc, [cell]: (acc[cell] || 0) + 1}
  }
}, {})


arr.forEach((cell, idx) => {
  if (idx < maxX || idx > (arr.length - maxX) || !(idx % maxX) || (idx % maxX === maxX -1)) {
    grouped[cell] = -1
  }
})

const values = Object.keys(grouped).map(key => grouped[key]).sort((a, b) => b - a)

console.log(`The biggest non-infinite area is ${values[0]} units large`)