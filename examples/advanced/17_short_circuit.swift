// Short-circuit operators `&&` and `||` are control flow in disguise: the
// right operand may not run. Whether you split them into separate CFG nodes
// is a design decision. Here it matters, because the right operand has a side
// effect through an `inout` call.

func check(_ value: inout Int) -> Bool {
    value += 1
    return value > 3
}

func shortCircuit(_ a: Int, _ flag: Bool) -> Int {
    var calls = a
    var hits = 0
    if flag && check(&calls) {
        hits += 1
    }
    if !flag || check(&calls) {
        hits += 10
    }
    let t = calls > 5 ? hits : -hits
    return t
}
