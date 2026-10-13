// Straight-line code: no branches at all.
// CFG: a single chain of nodes.
// DDG: c depends on a and b; d depends on c and a; the reassignment of a
// kills the first definition, so the final return sees only the second one.

func straightLine() -> Int {
    var a = 1
    let b = 2
    let c = a + b
    let d = c * a
    a = d - b
    return a + c
}
