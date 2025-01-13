#include "lexer/lexer.h"
#include "debug/debug.h"
#include "debug/token_string.h"
#include <stdio.h>

// For lexer print debugging
void print_token(Token token) {
	if (token.value) {
		debug_print(DEBUG_LEXER, "%s, %s\n",
				token_type_to_string(token.type), token.value);
	} else {
		debug_print(DEBUG_LEXER, "%s\n", token_type_to_string(token.type));
	}
}

int main(int argc, char** argv) {
	if (argc != 2) {
		fprintf(stderr, "Usage: %s <filename>\n");
		return 1;
	}
	size_t i;
	debug_init();  // Set correct flags based on environment variables

	// Read the file
	FILE* file = fopen(argv[1], "r");
	if (!file) {
		perror("Error opening file");
		return 1;
	}

	fseek(file, 0, SEEK_END);
	long size = ftell(file);
	fseek(file, 0, SEEK_SET);

	char* buffer = malloc(size + 1);
	fread(buffer, 1, size, file);
	buffer[size] = '\0';
	fclose(file);

	// Tokenise the file
	Token* tokens = lex(buffer);

	// Print lex output if debug category enabled
	for (i = 0; tokens[i].type != EOF; i++) {
		print_token(tokens[i]);
	}

	// Free the tokens and their string values
	for (i = 0; tokens[i].type != EOF; i++) {
		free(tokens[i].value);
	}
	free(tokens);

	return 0;
}
