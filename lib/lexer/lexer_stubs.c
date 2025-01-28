#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/intext.h>
#include <caml/bigarray.h>
#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"

CAMLprim value c_lex(value ocaml_input) {
	CAMLparam1(ocaml_input);
	const char* input = String_val(ocaml_input);
	Token* tokens = lex(input);

	// Allocate OCaml array (tuple) for tokens
	int count = 0;
	while (tokens[count].type != TOK_EOF) count++;
	CAMLlocal1(ocaml_tokens);
	ocaml_tokens = caml_alloc_tuple(count);

	// Convert C token structs to OCaml token tuples and store in the array
	for (int i = 0; i < count; i++) {
		CAMLlocal1(token_tuple);
		token_tuple = caml_alloc_tuple(2);
		Store_field(token_tuple, 0, Val_int(tokens[i].type));
		Store_field(token_tuple, 1, caml_copy_string(
					tokens[i].value ? tokens[i].value : ""));
		Store_field(ocaml_tokens, i, token_tuple);
	}

	free_tokens(tokens);
	CAMLreturn(ocaml_tokens);
}
