# LAB4

**How to run?** Run the `main` function inside module `Lab4` too run all tests.
Note that we edited the Lecture4.hs file to prevent conflicts with our own show function in exercise 9.

## --== EXERCISE 1 ==--
No mayor questions came up, no questions came up that could not be answeredamongst ourselves.

## --== EXERCISE 3 ==--
``` 
--== Set Operations ==--
Generator from scratch tests
Intersection:   +++ OK, passed 100 tests.
Union:      +++ OK, passed 100 tests.
Difference:     +++ OK, passed 100 tests.

QuickCheck tests
Intersection:   +++ OK, passed 100 tests.
Union:      +++ OK, passed 100 tests.
Difference:     +++ OK, passed 100 tests. 
```

## --== EXERCISE 4 ==--
No questions arose that probably won't be answered by closer study of the material.

## --== EXERCISE 7 ==--
```
--== Testing Symmetric and Transitive Closure ==--

Symmetric Closure tests
Symmetric elements exist in closure:    +++ OK, passed 100 tests.
Closure element have origin:        +++ OK, passed 100 tests.

Transitive Closure tests
Transitives don't change:       +++ OK, passed 100 tests.
Transitive closure preserves elements:  +++ OK, passed 100 tests.
Transitive connections:         +++ OK, passed 100 tests.
```

## --== EXERCISE 8 ==--
```
+++ OK, failed as expected. Falsifiable (after 3 tests and 2 shrinks):
[(0,1)]
+++ OK, failed as expected. Falsifiable (after 3 tests and 3 shrinks):
[("a","")]
The tests fail, as such we can conclude (R^-1)^+ /= (R^+)^-1
Counterexample: R=[(1,0)]: (R_r)^+ = [(1,0),(0,1),(1,1),(0,0)], (R^+)_r = [(1,0),(0,1)]
```

## --== EXERCISE 9 ==--
```
Calling show on the fib Statement:

x = 0
y = 1

while (n > 0) {
	z = x
	x = y
	y = (z + y)
	n = (n - 1)
}

 Result of lexer: 

[TokenV "x",TokenAss,TokenI 0,TokenV "y",TokenAss,TokenI 1,TokenWhile,TokenOP,TokenV "n",TokenGt,TokenI 0,TokenCP,TokenOC,TokenV "z",TokenAss,TokenV "x",TokenV "x",TokenAss,TokenV "y",TokenV "y",TokenAss,TokenOP,TokenV "z",TokenAdd,TokenV "y)",TokenV "n",TokenAss,TokenOP,TokenV "n",TokenSubtr,TokenI 1,TokenCP,TokenCC]
```

