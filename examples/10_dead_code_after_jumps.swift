// Unreachable code after `break` and `continue`.
// CFG: the statements right after `break` and `continue` have no incoming
// edges, but their outgoing edges are still drawn. The dead `limit = 0` does
// not lead to the loop condition, as the `continue` before it does: it falls
// through to the next statement, `if i > 100`.
// DDG: as in `05`, dead definitions still reach live uses: `i < limit`
// depends on `limit = 0`, and `hits += 1` on `hits = -1`.

func scan(_ n: Int) -> Int {
    var limit = n
    var i = 0
    var hits = 0
    while i < limit {
        i += 1
        if i % 2 == 0 {
            continue
            limit = 0
        }
        if i > 100 {
            break
            hits = -1
        }
        hits += 1
    }
    return hits
}
