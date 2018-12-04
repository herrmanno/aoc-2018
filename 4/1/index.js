const input = require("fs").readFileSync("input.txt").toString().split("\n").sort()

let sums = {}
let gID = null

for (let line of input) {
    let [_, time, ...rest] = line.split(" ")
    time = time.slice(0, -1)
    line = rest.join(" ")
    let m = null
    if ((m = line.match(/Guard #(\d+)/), m)) {
        let [_, id] = m
        gID = id
        sums[gID] = sums[gID] || []
    } else if ((m = line.match(/falls/), m)) {
        sums[gID].push([time])
    } else {
        sums[gID][sums[gID].length - 1].push(time)
    }
}

let totals = Object.keys(sums).map(id => {
    const total = sums[id].reduce((sum, range) => {
        const [
            [h1, m1],
            [h2, m2]
        ] = range.map(s => s.split(":").map(Number))
        const time = (h2 + (h1 > h2 ? 24 : 0) * 60 + m2) - (h1 * 60 + m1)
        return sum + time
    }, 0)
    return { id, total }
})

const maxGuard = totals.sort((a, b) => a.total - b.total).reverse()[0]

const arr = new Array(60).fill(0)
sums[maxGuard.id].forEach(range => {
    let [minStart, minStop] = range.map(s => +s.split(":")[1])
    do {
        arr[minStart]++
        minStart = (minStart + 1) % 60
    } while (minStart != minStop)
})

const maxMinute = arr.map((val, idx) => [val, idx]).sort((a, b) => b[0] - a[0])[0][1]

console.log(`Multiplication of guard #${maxGuard.id} and minute ${maxMinute} ${+maxGuard.id * maxMinute}`)