# CS381-Panda Cubs

## Team Members

-   Aaron Galati
-   Kawin Pechetratanapanit
-   Weijie Mo
-   Chuan Cheng

## Introduction

Language Name: Pandascript
Language Paradigm: Imperative

Pandascript is a general purpose, minimalistic, C-like abstract syntax that focuses on expressibility. The language features implicit type conversion between all data types.

For example, this statement below is a perfectly valid code:

    Add (I 5) (F 5.0)

... and so is also a valid statement.

    If (Eq (I 10) (B True))

## Language Design

### Core

-   [x] Int and Float primitives
-   [x] Built-in arithmatics
-   [x] If-then-else conditional
-   [x] While loops.
-   [x] Implicit conversion between Int and Float.
-   [x] Function decaration and evaluation
-   [x] First class functions

These features were chosen as core features as they are necessary to the safety of the language. Without these, many features would become unusable in the language or unsafe.

### Sugar

-   [x] Boolean type True/False is a sugar for Integer 0/1 respectively
-   [x] Incrementing/Decrementing by one
-   [x] For loops

These features were chosen as sugar features as they simply make it easier to write programs in the language.

### Library

-   [x] String concatenation
-   [x] Swap two variables

These feature was chosen as a library feature because it is core functionality provided as a program as well as a basic feature that can be created in the language.

We have satisfied the constraints of the feature menu by having basic data types and operations on them such as addition, subtraction, multiplication, and division. We satisfy conditionals with if-then-else statements accounted for. We satsify the recursion/loops section by having while loops. We have a way to set variable names, thus satisfying the fourth constraint. We can handle functions that can take parameters which satisfies the fifth constraint. Finally, for the additional features we have satisfied the string constraint by have a String type with operations such as concatenation being possible on them. We have also implemented first class functions to satisfy the final additional feature needed.

### Safety Properties

This language uses a dynamic type system. The language tries its very best to do implicit conversion between types. If that fails, an exception is raised during runtime.

## Implementation

>`Expr := Expr`

>`Test := Expr -> Expr`

>`Statement := [Env] -> [Env]`


We decided on these semantic domains because we needed various levels to get from the core data types to manipulating them like a programming language with the environment in mind. This is what makes the functions of val, get, set, test, stmt, and run so particularly important as they help come together to make a straightforward language.


## Examples

### Good

Ex 1: cubs: This is a good example of first class functions at work. The expected output is for "result" to end up being 30 as seen below as new numbers are added to result, in this case, 10 and 20.

```haskell
cubs = [
        Let "result" (I 0),
        Func "add" ["y"] [
            Func "+" ["x"] [AddTo "result" (Get "x")],
            Call "+" [Get "y"]
        ],
        Call "add" [(I 10)],
        Call "add" [(I 20)]
    ]
```

```
Expected output:
[fromList [("add",Lambda ["y"] [Func "+" ["x"] [AddTo "result" (Get "x")],Call "+" [Get "y"]]),("result",I 30)]]
```

Ex 2: fib: This program performs a fibonacci sequence for a certain number provided during the Call. The expected output should have "result" as 55 in the end for an input of 10.

```haskell
fib = [
        Let "result" (I 0),
        Func "fib" ["count"] [
            Let "a" (I 0),
            Let "b" (I 1),
            For (Let "i" (I 0)) (Lt (Get "i") (Get "count")) (Inc "i") [
                Let "c" (Add (Get "a") (Get "b")),
                Set "a" (Get "b"),
                Set "b" (Get "c")
            ],
            Set "result" (Get "a")
        ],
        Call "fib" [(I 10)]
    ]
```

```
Expected output:
[fromList [("fib",Lambda ["count"] [Let "a" (I 0),Let "b" (I 1),For (Let "i" (I 0)) (Lt (Get "i") (Get "count")) (Inc "i") [Let "c" (Add (Get "a") (Get "b")),Set "a" (Get "b"),Set "b" (Get "c")],Set "result" (Get "a")]),("result",I 55)]]
```

Ex 3: swap: This program swaps two variables so that they represent the others value. The expected output is that with an input of an int 0 and int 1 corresponding to x and y, then x will be 1 and the y will be 0. Another example of this, but with an outside environment would be to copy out the first two "Let" statements and run with a pre built envrionment as shown in the readme using the pandaStack command.

```haskell
swap = [
        Let "resultx" (I 0),
        Let "resulty" (I 1),
        Func "swap" ["x", "y"] [
            Let "temp" (Get "x"),
            Set "resultx" (Get "y"),
            Set "resulty" (Get "temp")
        ],
        Call "swap" [(Get "resultx"), (Get "resulty")]
    ]
```

```
Epected output:
[fromList [("resultx",I 1),("resulty",I 0),("swap",Lambda ["x","y"] [Let "temp" (Get "x"),Set "resultx" (Get "y"),Set "resulty" (Get "temp")])]]
```


### Bad
Ex  1: The testing2 program is actually doing the same thing as testing1 program did, which is concatenate multiple strings. However when you run testing2 you would get an error, which said "Variable "i" doesn't exist", that is an Error constructor that we defined before(reference line:130), and the reason why would occur this error is that we need Let to bind a value to a variable, instead of Set, since Set is for reassignment.

```haskell
testing2 = [
        Let "s1" (Str "test"),
        For (Set "i" (I 0)) (Lt (Get "i") (I 10)) (Inc "i") [
            Set "s1" (Cat (Get "s1") (Get "s1"))
        ]
    ]
```
```
Current output:
*** Exception: (ERROR) Variable "i" doesn't exist
CallStack (from HasCallStack):
  error, called at main.hs:110:24 in main:Main
```

Ex  2: cubs3 is a program that is guaranteed to produce errors in haskell, as it is a good example of a bad use of the syntax of our language.

```haskell
cubs3 = [
        Let "x" (I, 0)]
        (Set "x" (Lte(I 3)(I 4)))   
```

```
Expected output:
Couldn't match expected type `Expr'
        with actual type `(Int -> Expr, Integer)'
Couldn't match expected type `Expr' with actual type `Test'
```
