// Calls from unreachable code.
// CFG: the call to `cleanup` sits after a `return`, so its node is gray, but
// the call edge is still drawn: call edges are collected from every node,
// reachable or not. `validate` is only called from dead code, so in practice
// it can never run, even though an edge points at it.

func validate(_ n: Int) -> Bool {
    return n >= 0
}

func cleanup(_ n: Int) {
    print(n)
}

func process(_ n: Int) -> Int {
    var result = n * 2
    if result > 100 {
        return 100
    }
    return result
    cleanup(result)
    if validate(result) {
        result = 0
    }
}
