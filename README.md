# braid

WIP programming language

## to print lexer output:
1. install OCaml, Opam (OCaml package manager) and Dune (OCaml build system)
2. follow all the post-installation instructions on the OCaml website
3. install the `stdint` module: `opam install stdint`
4. `dune build`
5. `dune exec bin/main.exe examples/hello.braid` (try the other files in `examples` too)
6. to clean up temporary files: `dune clean`
