// Overloading by argument labels.
// CFG: every call is resolved by name plus labels, so `scale(_:)`,
// `scale(_:by:)` and `scale(x:)` are three different targets. A nested call
// such as `scale(scale(v), by: 3)` produces an edge to each callee, and the
// two calls to `scale(x:)` on one line collapse into a single edge.
// `print` is not defined in the file, so it gets no call edge.

func scale(_ v: Int) -> Int {
    return v * 2
}

func scale(_ v: Int, by factor: Int) -> Int {
    return v * factor
}

func scale(x v: Int) -> Int {
    return v * 10
}

func combine(_ v: Int) -> Int {
    let a = scale(scale(v), by: 3)
    let b = scale(x: a) + scale(x: v)
    print(b)
    return a + b
}
