// Nested while loops: two back edges, one inside the other.
// DDG: `total` in the inner body depends on its definition before the outer
// loop and on itself (carried by both loops); `j` is reset on every outer
// iteration, so the inner condition sees both `var j = 1` and `j += 1`.

func tableSum(_ n: Int) -> Int {
    var total = 0
    var i = 1
    while i <= n {
        var j = 1
        while j <= n {
            total += i * j
            j += 1
        }
        i += 1
    }
    return total
}
