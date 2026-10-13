// A dead assignment whose value still flows into live code.
// CFG: `x = 100` and the loop condition are unreachable, but the condition's
// `true` edge leads back to the live loop head.
// DDG: reaching definitions follow every CFG edge, so `x > 10` and `x -= 1`
// get a dependency on the dead `x = 100` (drawn dashed). The analysis is not
// path-sensitive: it only knows the edge exists, not that it can never run.

func countDown(_ n: Int) -> Int {
    var x = n
    repeat {
        if x > 10 {
            return x
        }
        x -= 1
        return x
        x = 100
    } while x > 0
    return 0
}
