#include "lexer/lexer.h"
#include "debug/debug.h"
#include "debug/token_string.h"
#include <stdio.h>
#include <stdlib.h>

void print_token(Token token) {
	if (token.value) {
		debug_print(DEBUG_LEXER, "%s, %s\n", token_type_to_string(token.type), token.value);
	} else {
		debug_print(DEBUG_LEXER, "%s\n", token_type_to_string(token.type));
	}
}

int main(int argc, char** argv) {
	if (argc != 2) {
		fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
		return 1;
	}
	size_t i;
	debug_init();  // Set up debugging if `BRAID_DEBUG` is set

	// Read the file
	FILE* file = fopen(argv[1], "r");
	if (!file) {
		perror("Error opening file");
		return 1;
	}
	fseek(file, 0, SEEK_END);
	long size = ftell(file);
	fseek(file, 0, SEEK_SET);

	char* input = malloc(size + 1);
	fread(input, 1, size, file);
	input[size] = '\0';
	fclose(file);

	// Tokenise the file
	Token* tokens = lex(input);

	// Print lex output if debug category enabled
	for (i = 0; tokens[i].type != EOF_TOKEN; i++) {
		print_token(tokens[i]);
	}

	// Free the tokens and their string values
	for (i = 0; tokens[i].type != EOF_TOKEN; i++) {
		free(tokens[i].value);
	}
	free(tokens);

	return 0;
}
