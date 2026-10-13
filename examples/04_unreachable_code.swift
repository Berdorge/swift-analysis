// Unreachable code: statements after `return`, and a `repeat` whose body
// always returns, so its condition is never evaluated.
// CFG: the dead nodes are not reachable from the entry, although the dead
// condition still has an edge back to the loop head.

func firstPositive(_ a: Int, _ b: Int) -> Int {
    var result = a
    repeat {
        if result > 0 {
            return result
        }
        result = b
        return result
    } while result < 0
    result = 0
    return result
}
