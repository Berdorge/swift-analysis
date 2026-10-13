// A simple branch and join point.
// CFG: a diamond. DDG: `result` at the return has two reaching definitions,
// one from each branch.

func maxOf(_ x: Int, _ y: Int) -> Int {
    var result = 0
    if x > y {
        result = x
    } else {
        result = y
    }
    return result
}
