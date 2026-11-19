// `for`-`in` over sequences that are not ranges: an array parameter and a
// `stride(..)` call.
// CFG: such a sequence goes through its iterator, looking one element ahead:
// `var $itN = seq.makeIterator()` and `var $nextN = $itN.next()` before the
// loop, the condition `$nextN != nil`, and `let v = $nextN!;
// $nextN = $itN.next()` at the start of the body. `stride(from:to:by:)` comes
// from the standard library, so it has no call edge, and it is called once.
// DDG: `values` and `step` are used only by `makeIterator()`; the body depends
// on them through the loop variable. `$nextN = $itN.next()` depends on itself,
// because a method call is assumed to mutate its receiver.

func sumOfPositives(_ values: [Int]) -> Int {
    var sum = 0
    for v in values {
        if v > 0 {
            sum += v
        }
    }
    return sum
}

func countMultiples(of step: Int, below limit: Int) -> Int {
    var count = 0
    for m in stride(from: 0, to: limit, by: step) {
        if m % 2 == 1 {
            count += 1
        }
    }
    return count
}
