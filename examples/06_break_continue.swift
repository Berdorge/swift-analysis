// `break` and `continue` in a `while` loop.
// CFG: `continue` jumps back to the loop condition; `break` jumps to the
// statement after the loop, where it meets the condition's `false` edge.
// DDG: `return found` depends both on `var found = -1` (the loop ran out) and
// on `found = i` (the loop was left through `break`).

func firstOddMultiple(of k: Int, upTo limit: Int) -> Int {
    var found = -1
    var i = 0
    while i < limit {
        i += 1
        if i % 2 == 0 {
            continue
        }
        if i % k == 0 {
            found = i
            break
        }
    }
    return found
}
