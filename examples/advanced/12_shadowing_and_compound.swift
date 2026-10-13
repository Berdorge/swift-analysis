// Subtle data dependencies:
//  * shadowing: the inner `x` is a different variable from the outer one;
//  * compound assignment `+=` both uses and defines;
//  * tuple destructuring defines two variables at once;
//  * swapping through a tuple uses both before defining both.

func subtle(_ input: Int) -> Int {
    var x = input
    var y = 0
    if x > 10 {
        let x = 5
        y += x
    }
    x += y
    var (p, q) = (x, y)
    (p, q) = (q, p)
    return p - q
}
