const input = require("fs").readFileSync("input.txt").toString().split("\n")

input.forEach((line1, idx) => {
    [...input.slice(0, idx), ...input.slice(idx + 1)].forEach(line2 => {
        let diffN = [...line1].reduce((acc, char, idx) => acc + +(char != line2[idx]), 0)
        if (diffN === 1) {
            let chars = [...line1].filter((char, idx) => char === line2[idx]).join("")
            console.log(`The similiar package name chars are “${chars}“`)
        }
    })
})