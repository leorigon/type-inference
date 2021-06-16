# Type Inference Algorithm

This repository contains an old homework from the type systems discipline of the Master's Degree in Applied Computing course of UDESC.
The present inference algorithm was based on the [Hindley–Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)


## requirements:

- Stack;
- Some text editor.

## to run:

Create a file with the lambda expression (e.g. `\x -> x`) and type ```stack build and stack exec type-inference```
on terminal. After this, put the name of the file and, if the file's name was right, the algorithm will give the type of the expression.

## TODO:
- Create a set of tests using some haskell tests library;
- Allow more than one expression in each file.