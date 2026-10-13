// Recursive calls: call edges that form cycles.
// CFG: `factorial` has an edge back to its own entry; `isEven` and `isOdd`
// call each other, so their call edges form a cycle between two clusters.
// `unused` is never called, so nothing points at its cluster.

func factorial(_ n: Int) -> Int {
    if n <= 1 {
        return 1
    }
    return n * factorial(n - 1)
}

func isEven(_ n: Int) -> Bool {
    if n == 0 {
        return true
    }
    return isOdd(n - 1)
}

func isOdd(_ n: Int) -> Bool {
    if n == 0 {
        return false
    }
    return isEven(n - 1)
}

func unused(_ n: Int) -> Int {
    return factorial(n) + 1
}
