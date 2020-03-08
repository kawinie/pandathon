# CS381-Panda Cubs

## Team Members

-   Aaron Galati
-   Kawin Pechetratanapanit
-   Weijie Mo
-   Chuan Cheng

## Introduction

Language Name: Pandathlon
Language Paradigm: Imperative

Pandathlon is a general purpose, minimalistic, C-like abstract syntax that focuses on expressibility. The language features implicit type conversion between all data types.

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
-   [ ] Function decaration and evaluation
-   [ ] First class functions

These features were chosen as core features as they are necessary to the safety of the language. Without these, many features would become unusable in the language or unsafe.

### Sugar

-   [x] Boolean type True/False is a sugar for Integer 0/1 respectively
-   [x] Incrementing/Decrementing by one

These features were chosen as sugar features as they simply make it easier to write programs in the language.

### Library

-   [x] String concatenation

This feature was chosen as a library feature because it is core functionality provided as a program.

We have satisfied the constraints of the feature menu by having basic data types and operations on them such as addition, subtraction, multiplication, and division. We satisfy conditionals with if-then-else statements accounted for. We satsify the recursion/loops section by having while loops. We have a way to set variable names, thus satisfying the fourth constraint. We have functions planned out which satisfies the fifth constraint. Finally, for the additional features we have satisfied the string constraint by have a String type with operations such as concatenation being possible on them. We have also planned to implement first class functions to satisfy the final additional feature needed. We plan to use first class functions to help move features towards the library level as well.

### Safety Properties

The language tries its very best to do implicit conversion between types. If that fails, an exception is raised.

## Implementation

> `val := Env -> Expr`

This is the semantic domain for the val function. It outputs a value given an expression. This could be anything from a string concatenation to addition and subtraction to true or false.

> `op :: (Expr, Expr) -> (Int -> Int -> Int) -> (Float -> Float -> Float) -> Expr`

This is the semantic domain for the op function. It is a helper function for val. It helps by determining what variables it needs to change to a float to prevent type errors.

> `set :: Env -> Expr -> Env`

This is the semantic domain for the set function. It either updates an existing value in the environment or inserts a new value into the environment.

> `get :: Env -> Var -> E`

This is the semantic domain for the get function. It gets a literal value from the environment or returns that the value does not exist.


> `test :: Env -> Bool`

This is the semantic domain for the test valuation function. It gives an evaluation of various mathematical operations such as equality and greater or lesser values returning

> `stmt :: Env -> Env`

This is the semantic domain for statement valuation function. It allows for core functionality commands to operate such as If-then blocks, while loops, and do blocks.

> `run :: Env -> [S] -> Env`

This is the semantic domain for the run function. This function lets a series of statements carry out their designated tasks.

> `panda :: [S] -> Env`

This is the semantic domain for the panda function. This function is for running a block of statements while starting with an empty environment.



We decided on these semantic domains because we needed various levels to get from the core data types to manipulating them like a programming language. This is what makes val, get, set, test, stmt, and run so particularly important as they help come together to make a straightforward language.
