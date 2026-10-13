// A while loop: the first back edge.
// DDG: `i` and `sum` inside the loop depend both on their initial definitions
// and on their own definitions from the previous iteration (loop-carried).

func sumUpTo(_ n: Int) -> Int {
    var sum = 0
    var i = 1
    while i <= n {
        sum += i
        i += 1
    }
    return sum
}
