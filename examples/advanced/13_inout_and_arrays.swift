// `inout` parameters and element updates.
// Should `arr[i] = v` count as a definition of `arr`? Is it a strong or a weak
// update? Does the call `bump(&counter)` define `counter`? Decide and document it.

func bump(_ value: inout Int) {
    value += 1
}

func fill(_ n: Int) -> [Int] {
    var arr = [Int](repeating: 0, count: n)
    var counter = 0
    for i in 0..<n {
        bump(&counter)
        arr[i] = counter * 2
    }
    return arr
}
