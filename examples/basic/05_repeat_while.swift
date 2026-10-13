// repeat-while: the body runs at least once, so the condition comes after it.

func digitCount(_ n: Int) -> Int {
    var value = n
    var count = 0
    repeat {
        count += 1
        value /= 10
    } while value != 0
    return count
}
