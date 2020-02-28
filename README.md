# Pandathlon Guide

## Team Members

-   Aaron Galati
-   Kawin Pechetratanapanit
-   Weijie Mo
-   Chuan Cheng

## Imperative

Pandathlon is a general purpose, minimalistic, C-like abstract langauge that focuses on expressibility. The language features implicit type conversion between all data types.

## How to use

The language is run on GHCi. The only module to load is main.hs 

The following important functions can be used in the language:

>Set "x" (I 5)

This sets a variable named "x" to be an integer of 5. This could also be set to a float with (F 5.0), or a bool with (B True), or a String with (Str "test").

>Get "x"

This retuns the current value in a variable, if there is one.

>Add (value) (value)

This statement will add the two values together. Sub, Mul, Div, can also be used.

>Lte (value) (value)

This checks if the first value is less than or equal to the second. Gte, Lt, Gt, can also be used for their respective comparisons.

>Do [statements]

This allows a user to continue within a while loop or an if-then block using a series of further programatic statements.

>While (comparison) (Do [statements])

This allows a user to create a while loop with a comparison of values in the parentheses to loop with a block of code. This is followed by a Do block.

>If (condition) (Do [statements]) (Do [statements])

This allows a user to enter an if-then block that starts in on the first Do block if the condition is met otherewise, it moves to the second Do block.

For now, programs can be written by defining it by a name and then proceding to write it in brackets. For example:

<pre>
test = [  
    Set "x" (I 5),  
    Dec "x"  
]
</pre>

Then, this program can be called by using:

>panda test

Which should return:
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