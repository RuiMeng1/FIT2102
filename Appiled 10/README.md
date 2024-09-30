---
title: Week 10 Exercises
author: FIT2102 Programming Paradigms
margin: 1inch
---

## Requirements

Your quest this week is to embark on a coding journey and conquer the challenges presented by the exercises. Follow the instructions, ensuring that every code file is complete, and every test is conquered. Ensure all compilation warnings and errors are fixed, to ensure nothing stands in the way of your triumph.

May your code be resilient, your logic impeccable, and your debugging skills sharp. This quest is not merely about completion but about mastery. As you navigate through the exercises, tackle each challenge with determination and finesse.

We wish you the best of luck in completing this coding adventure. May your editor be swift, your compiler friendly, and your tests evergreen. Your success awaits at the end of this journey, where bug-free code and passing tests shall be your testament.

May the odds be ever in your favour.

`MaybeMonad.hs` will ask you to write `bind` and to write code to compare case syntax, bind and do notation.

`MonadParser` will ask you to write `bind` for parser and explore where monadic parser gives us more power compared to previous.

`SocketIO` you will use the Monad typeclass to help with Socket communication.

To test your SocketIO function, open the `Javascript` folder, and run the webpage (`npm install` and `npm run dev`). In another terminal run `stack run` Click the button, and see if you get the correct output!

### Optional Exercises

`ParserExercises` will focus on creating a series of useful functions to help you with calculator.

`Helper.hs` contains a bunch of helper functions which might be useful to you along your quest. There are no questions to be completed with this file, but it will contains solutions from previous weeks, parsers which you have created before, to help you with the calculator parser.

`Calculator.hs` you will be creating a parser which can handle basic mathematical expressions. You will then use this parser for some interesting tasks. Firstly, using IO to evaluate all expressions in a file. Secondly, writing a REPL to handle user input and create an interesting program.

`FileIO` is an optional but very interesting exercise focusing on more FileIO operations. Here, you will be playing with a series of monadic operations to do interesting tasks.

## Some useful operators and functions

These are some useful operators and functions. If you don't know what they do, feel free to look them up on [Hoogle](https://hoogle.haskell.org).

```
Functor. Replacing or otherwise interacting with a parsed value.
- fmap, (<$>), ($>), (<$)

Applicative. Chaining together or using multiple Parser values.
- (*>), (<*), (<*>)
- pure, liftA2, liftA3, liftA4
- replicateM, traverse, sequence, replicate
- (<|>), some, many, foldr
```
