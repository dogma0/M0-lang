Built a toy lisp compiler. Despite being a toy project, it's still quite interesting imo. 
The interesting bit is it's small. 1500 LOC (Racket) and 50% is test. 
Also, since it's small and the compiler logically separates the compilation steps, it's pretty easy to understand what's going on.

# More details
---
The compiler does 4 steps to compile an expression.
E.g. `not 5` will be written with the prefix notation `(not 5)`.

And we write `(not 5)` as `(if 5 0 1)` in this example, because it's easier to explain to this way. 
Side note: `not` can be defined in terms of the primitive `if`, and shipped in a runtime library.

The compilation stages will be:
1. first compilation step

Simply wrap expression in a tag `(L0: ...)` and literal in the datum tag `(L0: datum ...)` 

`(M0→L0 '(if 5 0 1)) ; -> '(L0: if (L0: datum 5) (L0: datum 0) (L0: datum 1))`

2. second compilation step

write the primitive `if` as a function of 1 argument, 0 is the index into the argument list

`(L0→L1 (M0→L0 '(if 5 0 1))) ; -> '(L1: if 0 (L1: datum 5) (L1: datum 0) (L1: datum 1))`

3. third compilation step

this is the more interesting step, this code basically looks like assembly

The `not 5 `expression is compiled to the code below, and when it runs, it does: 
1. set the name `result` to be `5`
2. run `jump_false`, which jumps to label `else_0` if `result` is false
3. sets the name result to `1` (i.e. true) and jump to end_0 to finish the routine

```
; compiled code
(compiled:L2
 '((L2: set_result 5)
   (L2: jump_false else_0)
   (L2: set_result 0)
   (L2: jump end_0)
   (L2: label else_0)
   (L2: set_result 1)
   (L2: label end_0))
 '())
 ```
