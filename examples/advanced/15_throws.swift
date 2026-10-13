// Errors: `throw`, `try`, and `do`/`catch`.
// Any `try` expression can transfer control to the matching `catch` clause
// (or out of the function if nothing catches it), which adds extra edges.

enum ParseError: Error {
    case empty
    case negative(Int)
}

func parsePositive(_ s: String) throws -> Int {
    if s.isEmpty {
        throw ParseError.empty
    }
    let n = Int(s) ?? 0
    if n < 0 {
        throw ParseError.negative(n)
    }
    return n
}

func total(_ inputs: [String]) -> Int {
    var sum = 0
    var errors = 0
    for s in inputs {
        do {
            let v = try parsePositive(s)
            sum += v
        } catch ParseError.negative(let n) {
            sum += n
            errors += 1
        } catch {
            errors += 1
        }
    }
    return sum - errors
}
