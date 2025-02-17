# braid

WIP programming language

the parser is not complete. if you want to build the lexer part of the project, you can just comment out all the lines in `lib/parser/parser.ml` (or figure out how to exclude it from the build if you can be bothered)

## to print lexer output:
1. install OCaml, Opam (OCaml package manager) and Dune (OCaml build system)
2. follow all the post-installation instructions on the OCaml website
3. install the `stdint` module: `opam install stdint`
4. `dune build`
5. `dune exec bin/main.exe examples/hello.bd` (try the other files in `examples` too)
6. to clean up temporary files: `dune clean`
