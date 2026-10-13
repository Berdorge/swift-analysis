// `if` without `else`, plus an `else if` chain.
// The initial definition of `sign` reaches the return through the implicit
// fall-through edge of the last condition.

func classify(_ n: Int) -> Int {
    var sign = 0
    if n > 0 {
        sign = 1
    } else if n < 0 {
        sign = -1
    }

    var magnitude = n
    if n < 0 {
        magnitude = -n
    }
    return sign * magnitude
}
