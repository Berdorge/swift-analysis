// `while true`: the analysis does not evaluate conditions, so `true` still
// gets a `false` edge.
// CFG: in `runUntilZero` the code after the loop is reached through `break`
// and also through that `false` edge. `forever` has no `break`, yet
// `return x` is still drawn as reachable. Swift does know that this loop
// never ends: the function would compile without the `return`.

func step(_ x: Int) -> Int {
    return (x * 7 + 3) % 10
}

func runUntilZero(_ seed: Int) -> Int {
    var x = seed
    var count = 0
    while true {
        x = step(x)
        if x == 0 {
            break
        }
        count += 1
    }
    return count
}

func forever(_ seed: Int) -> Int {
    var x = seed
    while true {
        x = step(x)
    }
    return x
}
