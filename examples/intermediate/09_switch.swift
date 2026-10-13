// `switch` with multiple patterns per case, ranges, `where`, `default`
// and `fallthrough`. Swift cases do not fall through implicitly.

func describe(_ n: Int) -> String {
    var label = ""
    var bonus = 0
    switch n {
    case 0:
        label = "zero"
    case 1, 2, 3:
        label = "small"
        bonus = 1
    case let x where x < 0:
        label = "negative"
        bonus = -x
    case 4..<10:
        label = "medium"
        fallthrough
    case 10:
        bonus += 10
    default:
        label = "large"
    }
    return label + String(bonus)
}
