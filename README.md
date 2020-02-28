[![Code Size](https://img.shields.io/github/languages/code-size/kawinie/cs381-final-project)](https://img.shields.io/github/languages/code-size/kawinie/cs381-final-project)

# Pandathon 
s
Pandathlon is a general purpose, minimalistic, C-like abstract langauge that focuses on expressibility. The language features implicit type conversion between all data types.

## Getting Started

Pandathon supports the following core features

- [x] Int, Float, Bool, and String primitives
- [x] Built-in basic arithmatics
- [x] If-then-else conditional
- [x] While loops
- [x] Implicit conversion between all types
- [ ] Function decaration and evaluation
- [ ] First class functions

### Assigning variables

`Set` command is used to assign a value to variable. The following example binds "x" to integer 5.

```haskell
Set "x" (I 5) // x = 5
```

The variable can be reassigned to a different type the same way. Pandathon supports `I Int`, `F Float`, `B Bool`, `Str String`.

```haskell
Set "x" (F 5)     // reassigning x to floating point number 5.0`
Set "x" (Str "5") // x is now a string
```

### Getting value

`Get` command is used to obtain the value bounded to a variable.

```haskell
Get "x" // -> Str "5"
```

### The rest 

```haskell
Add (value) (value)
```

This statement will add the two values together. Sub, Mul, Div, can also be used.

>`Lte (value) (value)`

This checks if the first value is less than or equal to the second. Gte, Lt, Gt, can also be used for their respective comparisons.

>`Do [statements]`

This allows a user to continue within a while loop or an if-then block using a series of further programatic statements.

>`While (comparison) (Do [statements])`

This allows a user to create a while loop with a comparison of values in the parentheses to loop with a block of code. This is followed by a Do block.

>`If (condition) (Do [statements]) (Do [statements])`

This allows a user to enter an if-then block that starts in on the first Do block if the condition is met otherewise, it moves to the second Do block.

For now, programs can be written by defining it by a name and then proceding to write it in brackets. For example:

```haskell
test = [  
    Set "x" (I 5),  
    Dec "x"  
]
```

Then, execute the program by calling `panda test`

Which outputs:
>fromList [("x", I 4)]

as x was decremented by one.

There are more good and thorough examples of how to use the language inside of main.hs

They can be ran with:

Good examples:
>panda cubs

expected output: 

>fromList [("count",I 6),("s",Str "Panda Panda 999.0"),("x",F 13.0)]

>panda cubs2

expected output: 

>fromList [("count",I 9),("x",F 9.765625)]

Bad examples:  
These must be uncommented in main.hs. Will cause errors

>panda cubs3

>panda cubs4

## Authors

- Kawin Pechetratanapanit
- Aaron Galati
- Weijie Mo
- Chuan Cheng
