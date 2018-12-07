const input = require("fs").readFileSync("./input.txt").toString().split("\n")

const conditions = input.reduce((obj, line) => {
    const [_, preCondition, step] = line.match(/([A-Z])/gm)
    return {
        ...obj,
        [step]: [...(obj[step] || []), preCondition],
        [preCondition]: obj[preCondition] || [],
    }
}, {})

const order = []

while (order.length < Object.keys(conditions).length) {
    for (const key of Object.keys(conditions).sort()) {
        if (order.includes(key)) {
            continue
        } else {
            if (conditions[key].every(k => order.includes(k))) {
                order.push(key)
                break
            }
        }
    }
}

console.log(`The steps shoul be executed in order '${order.join("")}'`)