idea for auto diff:

An `Operator` is a `(Number ... -> (values Number (list-of Number)`
Represents a differentiable function. The function takes in its arguments and returns its results as usual. But it also returns the (partial) derivative of the result with respect to each argument.
CONSTRAINT: The length of the list of derivatives must be the same as the number of arguments received.

A `DNumber` is a `(cons Number (list-of (cons DNumber Number)`
Represents the result of a differentiable arithmetic computation. Contains a list of pairs of `DNumbers` and derivatives representing arguments to this computation and the (partial) derivative of the result with respect to that argument.

An operator can be wrapped to form a function of type `(DNumber ... -> DNumber)`


What about `x^x`?
The derivative is `x^x * (ln(x) + 1)`

For higher order derivatives, you could make operators return a `DNumber` for the first derivative and then you could compute the first derivative of that DNumber. But then would `DNumber`s store `DNumber`s for their derivatives? For `e^x`, the `DNumber` would be infinite. I guess you could use promises.
