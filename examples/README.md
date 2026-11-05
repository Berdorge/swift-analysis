# Example Swift programs

Inputs for testing the analyser, roughly ordered by difficulty.

- `01`–`03`: single functions with `else if` chains, nested loops, and branches inside loops.
- `04`–`05`: `repeat`-`while`, including a nested `repeat` and an empty body, and unreachable
  code with a dead assignment whose value still flows into live code.
- `06`–`10`: `break` and `continue`: in `while`, in nested loops, in `repeat`-`while`, in
  `while true`, and with unreachable code right after them.
- `11`–`14`: several functions per file: overloading by argument labels, recursion,
  a small program of functions calling each other, and calls from dead code.

Every function in a file is analysed and drawn in its own box; call edges connect a call to the
callee's entry.
Each file starts with a comment describing what it exercises.
All files type-check with `swiftc -typecheck <file>`.
