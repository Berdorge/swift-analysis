// A small program: functions with loops calling each other.
// CFG: `isPrime` has three exits: an early return before its loop, one
// inside it, and the final one. `nthPrime` loops with `while true` and leaves
// through `break`, skipping composite numbers with `continue`. Calls appear
// in conditions (`if !isPrime(n)`, `while sumOfPrimes(upTo: limit) < target`),
// so those call edges start at diamond nodes.
// DDG: per function only; values passed to a call are not tracked into the
// callee.

func isPrime(_ n: Int) -> Bool {
    if n < 2 {
        return false
    }
    var d = 2
    while d * d <= n {
        if n % d == 0 {
            return false
        }
        d += 1
    }
    return true
}

func nthPrime(_ k: Int) -> Int {
    var count = 0
    var n = 1
    while true {
        n += 1
        if !isPrime(n) {
            continue
        }
        count += 1
        if count == k {
            break
        }
    }
    return n
}

func sumOfPrimes(upTo limit: Int) -> Int {
    var sum = 0
    var n = 2
    while n <= limit {
        if isPrime(n) {
            sum += n
        }
        n += 1
    }
    return sum
}

func primeSumBound(_ target: Int) -> Int {
    var limit = 2
    while sumOfPrimes(upTo: limit) < target {
        limit *= 2
    }
    return limit
}
