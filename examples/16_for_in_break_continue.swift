// `break` and `continue` in `for`-`in`, including nested loops.
// CFG: the hidden index is advanced at the start of the body, right after the
// loop variable is bound, so `continue` can go straight to the condition; with
// the increment at the end of the body it would skip it and loop forever.
// `break` leaves only the inner loop and meets the inner condition's `false`
// edge.
// DDG: `return found` depends on `var found = -1` and on `found = i * j`.

func firstProductAbove(_ limit: Int, size: Int) -> Int {
    var found = -1
    for i in 1...size {
        if i % 2 == 0 {
            continue
        }
        for j in i..<size {
            if i * j > limit {
                found = i * j
                break
            }
        }
        if found >= 0 {
            break
        }
    }
    return found
}
