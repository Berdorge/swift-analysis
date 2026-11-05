// `break` and `continue` in nested loops: each one belongs to the innermost
// loop around it.
// CFG: the outer `break` and `continue` come before the inner loop, yet the
// inner loop does not pick them up: they lead to `return pairs + rows` and
// to `i < n`. The inner `break` leads to `rows += 1`, right after the inner
// loop, and the inner `continue` leads to `j < n`.

func countPairs(_ n: Int, limit: Int) -> Int {
    var pairs = 0
    var rows = 0
    var i = 0
    while i < n {
        i += 1
        if pairs > limit {
            break
        }
        if i % 3 == 0 {
            continue
        }
        var j = 0
        while j < n {
            j += 1
            if j == i {
                continue
            }
            if i + j > limit {
                break
            }
            pairs += 1
        }
        rows += 1
    }
    return pairs + rows
}
