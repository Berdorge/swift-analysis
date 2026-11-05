// `break` and `continue` in `repeat`-`while`.
// CFG: `continue` jumps forward to the condition, which comes after the body,
// not back to the start of the body.
// `once`: the body always reaches `break`, so the condition is unreachable,
// just as it would be after a `return`. `retry`: one `continue` before the
// `break` makes the condition reachable again.

func collatzSteps(_ start: Int, cap: Int) -> Int {
    var x = start
    var steps = 0
    repeat {
        steps += 1
        if steps > cap {
            break
        }
        if x % 2 == 0 {
            x = x / 2
            continue
        }
        x = 3 * x + 1
    } while x != 1
    return steps
}

func once(_ n: Int) -> Int {
    var x = n
    repeat {
        x += 1
        break
    } while x < 10
    return x
}

func retry(_ n: Int) -> Int {
    var attempts = 0
    repeat {
        attempts += 1
        if attempts < n {
            continue
        }
        break
    } while true
    return attempts
}
