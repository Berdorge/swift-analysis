// Closures. Intraprocedurally, a closure body is a separate function: either
// analyse it as its own CFG, or treat the closure literal as an opaque value
// that *uses* its captured variables. Note that `counter` is captured and
// mutated by `increment`, which an intraprocedural DDG of `makeTotals` cannot see.

func makeTotals(_ xs: [Int]) -> Int {
    var counter = 0
    let increment = { (by: Int) in
        counter += by
    }
    for x in xs {
        increment(x)
    }
    let doubled = xs.map { $0 * 2 }
    let positives = doubled.filter { value in value > 0 }
    return counter + positives.count
}
