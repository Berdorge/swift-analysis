// `guard` statements and multiple `return`s.
// Every `return` must connect to the function's single exit node.
// The `else` block of a guard must leave the scope.

func safeDivide(_ a: Int, _ b: Int) -> Int {
    guard b != 0 else {
        return 0
    }
    let q = a / b
    if q > 1000 {
        return 1000
    }
    return q
}
