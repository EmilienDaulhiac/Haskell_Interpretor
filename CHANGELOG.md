# Revision history for cas761interpretor

## 0.1.0.0 -- 2024-03-28

# cas761interpretor

This project implements interpreters for a simple language with the following features:

* booleans, if-then-else
* pairs and projections
* lambda, application
* integers, addition, multiplication, (unary) minus
* ordering (i.e. less than) on integers
* equality on integers
* a combinator for computing fixed points 'fix'

The interpreters are implemented in Haskell using the finally tagless style.

There is 4 of them:
* a usual programming language (i.e. that 'runs') (Interpreter.hs)
* an interpreter that computes the length of the program (LengthCounterInterpreter.hs)
* an interpreter that computes (using `Data.Text`) a valid Haskell representation of the program (HaskellInterpreter.hs)
* an interpreter that computes (using `Data.Text`) a "pretty-printed" version of the program (PrettyPrintInterpreter.hs)

## Building the project

To build the project, you will need to have the Haskell Cabal tool installed on your system. Once you have Cabal installed, navigate to the project directory in a terminal and run the following command:
```
cabal build
```
This will build the project.

## Testing the interpreters

The project includes a test suite that exercises the interpreters and checks their output for correctness. To run the test suite, navigate to the project directory in a terminal and run the following command:
```
cabal test
```
This will run the test suite and print the results to the console.

## Project structure

The project is organized as follows:

* `src/`: contains the Haskell source code for the interpreters
* `test/`: contains the test suite for the interpreters
* `cas761interpretor.cabal`: the Cabal file that specifies the project dependencies and build settings

The main entry point for the interpreters is the `src/Interpreter.hs` module, which defines the `Expr` data type and the `ExprAlg` typeclass. The other modules in the `src/` directory define instances of the `ExprAlg` typeclass for the different interpreters.

The test suite is defined in the `test/Test.hs` module, which imports the interpreter modules and defines a set of test cases for each interpreter.