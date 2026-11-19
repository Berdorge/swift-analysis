// `for`-`in` over ranges: `a..<b` excludes `b`, `a...b` includes it, and `_`
// drops the loop variable.
// CFG: the loop is desugared into a `while` over a hidden index:
// `var $iN = 0` and `let $endN = n` before it, the condition `$iN < $endN`
// (`<=` for `...`), and `let i = $iN; $iN += 1` at the start of the body.
// The upper bound is evaluated once. With `_` there is no `let`.
// DDG: `sum += i` depends on `let i = $iN`, which depends on both the initial
// `var $iN` and on `$iN += 1` from the previous iteration.

func sumBelow(_ n: Int) -> Int {
    var sum = 0
    for i in 0..<n {
        sum += i
    }
    return sum
}

func factorial(_ n: Int) -> Int {
    var result = 1
    for k in 1...n {
        result *= k
    }
    return result
}

func power(_ base: Int, _ exponent: Int) -> Int {
    var result = 1
    for _ in 0..<exponent {
        result *= base
    }
    return result
}
