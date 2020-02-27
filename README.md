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

`Add (I 5) (F 5)`

... and so is also a valid statement.

`If (Eq (I 10) (B True))`

## Language Design

### Core

-   Int and Float primitives
-   Built-in arithmatics
-   If-then-else conditional
-   While loops.
-   Implicit conversion between Int and Float.

### Sugar

-   Boolean type True/False is a sugar for Integer 0/1 respectively
-   Incrementing/Decrementing by one

### Library

## Implementation

`test :: Env -> T -> Bool`

This is the semantic domain for the test valuation function. It gives an evaluation of various mathematical operations such as equality and greater or lesser values returning

`stmt :: Env -> S -> Env`

This is the semantic domain for statement valuation function. It allows for core functionality commands to operate such as If-then blocks, while loops, and do blocks.

`run :: Env -> [S] -> Env`

This is the semantic domain for the run function. This function lets a series of statements carry out their designated tasks.

`panda :: [S] -> Env`

This is the semantic domain for the panda function. This function is for running a block of statements while starting with an empty environment.
