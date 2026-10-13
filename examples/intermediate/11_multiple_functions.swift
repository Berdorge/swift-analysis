// Several functions in one file. The analysis is intraprocedural, so you should
// emit one CFG and DDG per function and treat calls as opaque expressions
// that use their arguments.

func square(_ x: Int) -> Int {
    return x * x
}

func sumOfSquares(_ n: Int) -> Int {
    var acc = 0
    var i = 0
    while i < n {
        acc = acc + square(i)
        i += 1
    }
    return acc
}

func main() {
    let n = 10
    let result = sumOfSquares(n)
    print(result)
}
