// Optional binding: `if let` and `while let` introduce new definitions
// in the condition itself. `??` is an expression, not control flow,
// unless you decide to model it as short-circuiting.

func sumParsed(_ items: [String]) -> Int {
    var total = 0
    var iterator = items.makeIterator()
    while let s = iterator.next() {
        if let n = Int(s) {
            total += n
        } else {
            total -= 1
        }
    }
    let fallback = Int("42") ?? 0
    return total + fallback
}
