// A branch inside a loop: the join point of the `if` flows back to the
// loop condition.
// DDG: `x` in the condition and in both branches depends on its initial
// definition and on the definitions from both branches of the previous
// iteration.

func collatzSteps(_ start: Int) -> Int {
    var x = start
    var steps = 0
    while x != 1 {
        if x % 2 == 0 {
            x = x / 2
        } else {
            x = 3 * x + 1
        }
        steps += 1
    }
    return steps
}
