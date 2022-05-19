/*
    reg mapping:
    JS var  | Reg name in program
    --------|--------------------
    a       | '1'
    b       | '4'
    c       | '5'

    The idea is:
    The program basically runs the following loop(s)

    Instr. 6-13    
        Modifies reg 1,4 and 5
        After running this loop if checks if reg 1 < 256.
        - If reg 1 < 256 programm jumps to instr 28, comparing reg 5 with reg 0.
          If reg 5 is *not* equals to reg 0 reg the program jumps back to instr 6
          and reruns at whole.
          <- this is our change to control (by setting reg 0) when the program will halt
        - If reg >= 256 reg 1 = reg 4 = (reg 1 `div` 256) will be set and program will jump
          to instr 8, rerunning the whole loop (but without initialising reg 1 and reg 5 at
          start of the loop).
*/

// run outer loop (instr 6 - 13)
function f(reg) {
    let {a,b,c} = reg
    a = c | 65536
    c = 10678677
    b = a & 255
    c = c + b
    c = c & 16777215
    c = c * 65899
    c = c & 16777215
    return {a,b,c}
}


// run outer loop w/o initialization (instr 8 - 13)
function g(reg) {
    let {a,b,c} = reg
    b = a & 255
    c = c + b
    c = c & 16777215
    c = c * 65899
    c = c & 16777215
    return {a,b,c}
}

// run after loop: modify reg 1 and reg 4 before next loop round
function m(reg) {
    let {a,b,c} = reg
    b = ~~(a / 256) 
    a = b
    return {a,b,c}
}

function run1() {
    let reg = {a: 0, b: 0, c: 0}
    reg = f(reg)
    do {
        reg = g(m(reg))
    } while (reg.a >= 256)
    console.log(reg.c)
}

function run2() {
    const values = new Set()
    let reg = {a: 0, b: 0, c: 0}
    let lastC = 0

    while (true) {
        reg = f(reg)
        do {
            reg = g(m(reg))
        } while (reg.a >= 256)

        if (values.has(reg.c)) break

        lastC = reg.c
        values.add(reg.c)
    }

    console.log(lastC)
}

console.log("Part 1")
run1()
console.log("Part 2")
run2()
