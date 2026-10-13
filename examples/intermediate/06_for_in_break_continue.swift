// for-in over a range with `continue` and `break`.
// CFG: `continue` jumps to the loop header, `break` jumps to the loop exit.
// DDG: the loop variable `k` is defined by the loop header on every iteration.

func firstBigEvenSquare(limit: Int) -> Int {
    var found = -1
    for k in 0..<limit {
        if k % 2 != 0 {
            continue
        }
        let sq = k * k
        if sq > 50 {
            found = sq
            break
        }
    }
    return found
}
