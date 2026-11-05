// `repeat`-`while`: the body runs before the condition is checked.
// CFG: there is no separate loop head; the condition's `true` edge goes back
// to the first node of the body. In `countTicks` the body starts with another
// `repeat`, so the back edges of both loops lead to `ticks += 1`.
// `waitUntilReady` has an empty body: its condition loops to itself.

func countTicks(_ rounds: Int) -> Int {
    var left = rounds
    var ticks = 0
    repeat {
        repeat {
            ticks += 1
        } while ticks % 3 != 0
        left -= 1
    } while left > 0
    return ticks
}

func isBusy() -> Bool {
    return false
}

func waitUntilReady(_ n: Int) -> Int {
    repeat {
    } while isBusy()
    return n
}
