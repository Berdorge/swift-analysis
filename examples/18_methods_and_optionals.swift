// Method calls, `nil` and postfix `!`.
// Every method is assumed to mutate its receiver, so `stack.popLast()` and
// `result.append(v)` both use and redefine the variable they are called on.
// CFG: `drain` is the same look-ahead loop that `for`-`in` over an array
// desugars into: `popLast()` runs once before the loop and once at the end of
// each iteration. In `keepEven` the array is iterated with
// `var $itN = values.makeIterator()` and `$nextN = $itN.next()`.
// DDG: `top = stack.popLast()` depends on itself through `stack`;
// `result.append(v)` depends on itself through `result` and on `let v`, and
// `return result` depends both on `var result = initial` and on `append`.

func drain(_ values: [Int]) -> Int {
    var stack = values
    var sum = 0
    var top = stack.popLast()
    while top != nil {
        sum += top!
        top = stack.popLast()
    }
    return sum
}

func keepEven(_ values: [Int], appendingTo initial: [Int]) -> [Int] {
    var result = initial
    for v in values {
        if v % 2 == 0 {
            result.append(v)
        }
    }
    return result
}
