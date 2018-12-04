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
    const arr = new Array(60).fill(0)
    const ranges = sums[id]
    ranges.forEach(range => {
        let [minStart, minStop] = range.map(s => +s.split(":")[1])
        do {
            arr[minStart]++
            minStart = (minStart + 1) % 60
        } while (minStart != minStop)
    })
    const total = arr.map((val, idx) => [val, idx]).sort((a, b) => b[0] - a[0])[0]
    return { id, total }
})

totals.sort((a, b) => b.total[0] - a.total[0])

const guard = totals[0]

console.log(`Multiplication of guard #${guard.id} and minute ${guard.total[1]} ${+guard.id * guard.total[1]}`)