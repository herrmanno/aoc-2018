const input = require("fs").readFileSync("./input.txt").toString().split("\n")

const conditions = input.reduce((obj, line) => {
    const [_, preCondition, step] = line.match(/([A-Z])/gm)
    return {
        ...obj,
        [step]: [...(obj[step] || []), preCondition],
        [preCondition]: obj[preCondition] || [],
    }
}, {})

const workers = new Array(5).fill(0).map(() => [])
const doing = new Set()
const done = new Set()

let i = 0
while (done.size !== Object.keys(conditions).length) {
    for (const worker of workers) {
        if (worker[i - 1] && worker[i - 1] != worker[i]) {
            done.add(worker[i - 1])
        }
    }
    
    const freeWorkers = workers.filter(w => !w[i])

    const openJobs = Object.keys(conditions).sort()
        .filter(key => !doing.has(key))
        .filter(key => conditions[key].every(k => done.has(k)))

    for (const worker of freeWorkers) {
        const job = openJobs.shift()
        if (!job) break

        doing.add(job)
        for (let j = 0; j < i; j++) {
            worker[j] = worker[j] || null
        }
        worker.push(...new Array(60 + job.charCodeAt(0) - 64).fill(job))
    }

    // print(i)
    i++
}


const max = workers.sort((a, b) => b.length - a.length)[0]
console.log(`With 5 workers it takes ${max} seconds`)




function print(m) {
    if (m !== undefined) {
        console.log(m + "\t\t" + workers.map(w => w[m] || ".").join("\t"))
    } else {
        const max = workers.sort((a, b) => b.length - a.length)[0].length
        for (let i = 0; i < max; i++) {
            console.log(workers.map(w => w[i] || ".").join("\t"))
        }   
    }
}