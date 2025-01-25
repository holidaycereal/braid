# braid

WIP programming language

**current status**: i have written the lexer in C and it works. i want to write the parser in Haskell but setting up FFI bindings is so annoying. (it's not working.)

**to print lexer output**:
* `cd lexer`
* `gcc -o debug debug.c lexer.c helpers.c` (or replace `gcc` with whatever C compiler you have)
* `./debug examples/test.braid`

**to attempt to build and run the Haskell version**:
* install `ghc` and `cabal`. the recommended method for installation is to use GHCup.
* `cabal build`
* `cabal run braid lexer/examples/test.braid`
* watch as it segfaults
* `cabal clean` to remove all the temporary files
