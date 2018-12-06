const input = require("fs").readFileSync("./input.txt").toString().split("\n").map(line => line.split(",").map(Number))

const maxX = Math.max(...input.map(coord => coord[0]))
const maxY = Math.max(...input.map(coord => coord[1]))

const arr = new Array(maxX * maxY).fill(null)

const n = arr.reduce((sum, _, i) => {
  const x0 = i % maxX
  const y0 = (i - x0) / maxX
  const totalDistance = input
    .map(([x,y]) => Math.abs(x0 - x) + Math.abs(y0 - y))
    .reduce((dist1, dist2) => dist1 + dist2)

  return sum + (totalDistance < 10000)
})

console.log(`The area of points that have a summed up distance to all points less than 10000 it ${n} units large`)
