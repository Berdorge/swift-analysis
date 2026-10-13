# Example Swift programs

Inputs for testing the analyser, roughly ordered by difficulty.

- `basic/`: straight-line code, `if`/`else`, `while`, `repeat`-`while`.
  Everything you need for a first working CFG and DDG.
- `intermediate/`: `for`-`in`, `break`/`continue` (including labelled ones),
  `guard`, `switch` with `fallthrough`, optional binding, several functions per file.
- `advanced/`: tricky cases that force design decisions: shadowing, compound
  and tuple assignments, `inout`, `defer`, `throw`/`try`/`catch`, closures, and
  short-circuit operators.

Each file starts with a comment describing what it exercises.
All files type-check with `swiftc -typecheck <file>`.
