// `defer` runs when the enclosing scope exits, through *any* exit, including
// early returns. In the CFG this means duplicating or routing the deferred
// block onto every exit edge of the scope. Multiple defers run in reverse order.

func process(_ values: [Int]) -> Int {
    var log = 0
    defer {
        log += 100
    }
    defer {
        log += 1
    }
    for v in values {
        if v < 0 {
            return -1
        }
        log += v
    }
    return log
}
