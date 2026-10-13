// Nested loops with a labelled `break` and `continue`.
// The labels decide which loop the jump targets, so your CFG builder has to
// track a stack of enclosing loops, not just the innermost one.

func findPair(_ xs: [Int], target: Int) -> (Int, Int) {
    var a = -1
    var b = -1
    outer: for i in 0..<xs.count {
        if xs[i] < 0 {
            continue outer
        }
        for j in (i + 1)..<xs.count {
            if xs[i] + xs[j] == target {
                a = i
                b = j
                break outer
            }
        }
    }
    return (a, b)
}
