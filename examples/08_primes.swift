// A small program: functions with loops calling each other.
// CFG: `isPrime` has three exits: an early return before its loop, one
// inside it, and the final one. Calls appear in conditions (`if isPrime(n)`,
// `while countPrimes(upTo: limit) < k`), so those call edges start at diamond
// nodes, and in a compound assignment (`total += sumOfPrimes(upTo: n)`).
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

func countPrimes(upTo limit: Int) -> Int {
    var count = 0
    var n = 2
    while n <= limit {
        if isPrime(n) {
            count += 1
        }
        n += 1
    }
    return count
}

func nthPrimeBound(_ k: Int) -> Int {
    var limit = 2
    while countPrimes(upTo: limit) < k {
        limit *= 2
    }
    return limit
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

func sumOfPrimeSums(upTo limit: Int) -> Int {
    var total = 0
    var n = 2
    while n <= limit {
        total += sumOfPrimes(upTo: n)
        n += 1
    }
    return total
}
