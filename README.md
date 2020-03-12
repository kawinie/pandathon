[![Code Size](https://img.shields.io/github/languages/code-size/kawinie/cs381-final-project)](https://img.shields.io/github/languages/code-size/kawinie/cs381-final-project)

# Pandathon

Pandathon is a general purpose, minimalistic, C-like abstract langauge written in Haskell that focuses on expressibility. The language features implicit type conversion between all data types.

## Getting Started

Pandathon supports the following core features

- [x] Int, Float, Bool, and String primitives
- [x] Built-in basic arithmatics
- [x] If-then-else conditional
- [x] While loops
- [x] Implicit conversion between all types
- [x] Function decaration and evaluation
- [x] First class functions

### Binding a Variable

`Set` command is used to assign a value to variable. The following example binds "x" to integer 5.

```haskell
Set "x" (I 5) --> 5
```

The variable can be reassigned to a different type the same way. Pandathon supports `I Int`, `F Float`, `B Bool`, `Str String`.

```haskell
Set "x" (F 5)     -- reassigning x to floating point number 5.0`
Set "x" (Str "5") -- x is now a string
```

### Getting Value

`Get` command is used to retrieve the value bounded to a variable.

```haskell
Get "x" --> Str "5"
```

### Basic Arithmetics

Pandathon automatically performs implicit conversion between types if possible. This means that you can add Int and Float together or vice versa seemlessly.

```haskell
Add (I 5) (I 10) --> 15
Sub (F 5) (I 10) --> -15.0
Mul (I 5) (F 10) --> 50.0
Div (F 5) (F 10) --> 0.5
```

### Comparisons

```haskell
Set "x" (I 10)
Lt (Get "x") (F 10)  --> False
Lte (Get "x") (F 10) --> True
Gt (Get "x") (F 10)  --> False
Gte (Get "x") (F 10) --> True
Eq (Get "x") (F 10)  --> True
```

### Control Flow

This allows a user to continue within a while loop or an if-then block using a series of further programatic statements.

```haskell
Begin [statements]
```

This allows a user to create a while loop with a comparison of values in the parentheses to loop with a block of code. This is followed by a Do block.

```haskell
While (condition) [statements]
```

This allows a user to enter an if-then block that starts in on the first Do block if the condition is met otherewise, it moves to the second Do block.

```haskell
If (condition) [statements] [statements]
```

This allows a user to declare a function with arguments and what it will do:

```haskell
Func "name" ["arg1", "arg2"] [
    statements
]
```

This allows a user to call a declared function, naming the values for the arguments:

```haskell
Call "name" [(I 1), (I 2)]
```


For now, programs can be written by defining it by a name and then proceding to write it in brackets. Then, execute the program by calling `panda cubs`:

```haskell
-- Fibbonaci:
cubs = [
        Set "a" (I 0),
        Set "b" (I 1),
        Set "count" (I 0),
        While (Lt (Get "count") (I 10)) [
            Set "c" (Add (Get "a") (Get "b")),
            Set "a" (Get "b"),
            Set "b" (Get "c"),
            Inc "count"
        ]
    ]

main = do
    print (panda cubs) --> fromList [("a",I 55),("b",I 89),("c",I 89),("count",I 10)]
```

## Authors

- Kawin Pechetratanapanit
- Aaron Galati
- Weijie Mo
- Chuan Cheng
