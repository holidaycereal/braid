# braid

WIP programming language

## instructions for lexer printer thing
for my friends who im showing this to who dont know terminal stuff
- `git clone https://github.com/holidaycereal/braid`
- `cd braid/lexer`
- figure out which C compiler you have
```
which cc
which gcc
which clang
```
^^ try those commands and use whichever one doesnt give you an error message
- `cc main.c lexer.c word_type.c -o main` (substitute `cc` if needed)
- `./main examples/test.braid`

hopefully it works for you

you can compare the output to the file examples/test.braid to see what it's doing

and try messing with the code if u want!! (u will have to compile it again if u do btw)
