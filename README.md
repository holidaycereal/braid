# braid

WIP programming language

## instructions for lexer printer thing
for my friends who im showing this to who dont know terminal stuff
1. `git clone https://github.com/holidaycereal/braid`
2. `cd braid/lexer`
3. figure out which C compiler you have. try `which cc`, `which gcc`, `which clang`. use whichever one doesn't give you an error message
4. `cc main.c lexer.c word_type.c -o main` (substitute `cc` if needed)
5. `./main examples/test.braid`

hopefully it works for you. you can compare the output to the file `examples/test.braid` to see what it's doing

and try messing with the code if u want!! (u will have to compile it again if u do btw)
