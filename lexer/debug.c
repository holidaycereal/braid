#include "lexer.h"
#include <stdio.h>
#include <stdlib.h>

static inline void print_token(Token token) {
	switch (token.type) {
	case TOK_WORD_IF: printf("TOK_WORD_IF"); break;
	case TOK_WORD_THEN: printf("TOK_WORD_THEN"); break;
	case TOK_WORD_ELSE: printf("TOK_WORD_ELSE"); break;
	case TOK_WORD_ELIF: printf("TOK_WORD_ELIF"); break;
	case TOK_WORD_WHILE: printf("TOK_WORD_WHILE"); break;
	case TOK_WORD_FOR: printf("TOK_WORD_FOR"); break;
	case TOK_WORD_IN: printf("TOK_WORD_IN"); break;
	case TOK_WORD_BREAK: printf("TOK_WORD_BREAK"); break;
	case TOK_WORD_CONTINUE: printf("TOK_WORD_CONTINUE"); break;
	case TOK_WORD_MATCH: printf("TOK_WORD_MATCH"); break;
	case TOK_WORD_WITH: printf("TOK_WORD_WITH"); break;
	case TOK_WORD_SWITCH: printf("TOK_WORD_SWITCH"); break;
	case TOK_WORD_DYN: printf("TOK_WORD_DYN"); break;
	case TOK_WORD_TYPE: printf("TOK_WORD_TYPE"); break;
	case TOK_WORD_RECORD: printf("TOK_WORD_RECORD");
	case TOK_WORD_INCLUDE: printf("TOK_WORD_INCLUDE"); break;
	case TOK_WORD_IMPORT: printf("TOK_WORD_IMPORT"); break;
	case TOK_WORD_EXIT: printf("TOK_WORD_EXIT"); break;
	case TOK_WORD_AND: printf("TOK_WORD_AND"); break;
	case TOK_WORD_OR: printf("TOK_WORD_OR"); break;
	case TOK_WORD_XOR: printf("TOK_WORD_XOR"); break;
	case TOK_WORD_NOT: printf("TOK_WORD_NOT"); break;
	case TOK_PRIM_U8: printf("TOK_PRIM_U8"); break;
	case TOK_PRIM_U16: printf("TOK_PRIM_U16"); break;
	case TOK_PRIM_U32: printf("TOK_PRIM_U32"); break;
	case TOK_PRIM_U64: printf("TOK_PRIM_U64"); break;
	case TOK_PRIM_I8: printf("TOK_PRIM_I8"); break;
	case TOK_PRIM_I16: printf("TOK_PRIM_I16"); break;
	case TOK_PRIM_I32: printf("TOK_PRIM_I32"); break;
	case TOK_PRIM_I64: printf("TOK_PRIM_I64"); break;
	case TOK_PRIM_USIZE: printf("TOK_PRIM_USIZE"); break;
	case TOK_PRIM_ISIZE: printf("TOK_PRIM_ISIZE"); break;
	case TOK_PRIM_F32: printf("TOK_PRIM_F32"); break;
	case TOK_PRIM_F64: printf("TOK_PRIM_F64"); break;
	case TOK_PRIM_BOOL: printf("TOK_PRIM_BOOL"); break;
	case TOK_LIT_TRUE: printf("TOK_LIT_TRUE"); break;
	case TOK_LIT_FALSE: printf("TOK_LIT_FALSE"); break;
	case TOK_IDENT: printf("TOK_IDENT"); break;
	case TOK_LIT_INT_DEC: printf("TOK_LIT_INT_DEC"); break;
	case TOK_LIT_INT_HEX: printf("TOK_LIT_INT_HEX"); break;
	case TOK_LIT_INT_OCT: printf("TOK_LIT_INT_OCT"); break;
	case TOK_LIT_INT_BIN: printf("TOK_LIT_INT_BIN"); break;
	case TOK_LIT_FLOAT: printf("TOK_LIT_FLOAT"); break;
	case TOK_LIT_CHAR: printf("TOK_LIT_CHAR"); break;
	case TOK_LIT_STR: printf("TOK_LIT_STR"); break;
	case TOK_LIT_STR_RAW: printf("TOK_LIT_STR_RAW"); break;
	case TOK_ERR: printf("TOK_ERR"); break;
	case TOK_PAREN_L: printf("TOK_PAREN_L"); break;
	case TOK_PAREN_R: printf("TOK_PAREN_R"); break;
	case TOK_BRACKET_L: printf("TOK_BRACKET_L"); break;
	case TOK_BRACKET_R: printf("TOK_BRACKET_R"); break;
	case TOK_BRACE_L: printf("TOK_BRACE_L"); break;
	case TOK_BRACE_R: printf("TOK_BRACE_R"); break;
	case TOK_DOT: printf("TOK_DOT"); break;
	case TOK_COMMA: printf("TOK_COMMA"); break;
	case TOK_SEMICOLON: printf("TOK_SEMICOLON"); break;
	case TOK_COLON: printf("TOK_COLON"); break;
	case TOK_EQUALS: printf("TOK_EQUALS"); break;
	case TOK_VERT_LINE: printf("TOK_VERT_LINE"); break;
	case TOK_AMPERSAND: printf("TOK_AMPERSAND"); break;
	case TOK_BANG: printf("TOK_BANG"); break;
	case TOK_QUESTION: printf("TOK_QUESTION"); break;
	case TOK_CARET: printf("TOK_CARET"); break;
	case TOK_MINUS: printf("TOK_MINUS"); break;
	case TOK_PLUS: printf("TOK_PLUS"); break;
	case TOK_STAR: printf("TOK_STAR"); break;
	case TOK_SLASH: printf("TOK_SLASH"); break;
	case TOK_PERCENT: printf("TOK_PERCENT"); break;
	case TOK_LESS: printf("TOK_LESS"); break;
	case TOK_GREATER: printf("TOK_GREATER"); break;
	case TOK_COMP_EQ: printf("TOK_COMP_EQ"); break;
	case TOK_COMP_NE: printf("TOK_COMP_NE"); break;
	case TOK_COMP_LE: printf("TOK_COMP_LE"); break;
	case TOK_COMP_GE: printf("TOK_COMP_GE"); break;
	case TOK_ARROW: printf("TOK_ARROW"); break;
	case TOK_RETURN_ARROW: printf("TOK_RETURN_ARROW"); break;
	case TOK_FWD_COMPOSE: printf("TOK_FWD_COMPOSE"); break;
	case TOK_PLUS_EQUALS: printf("TOK_PLUS_EQUALS"); break;
	case TOK_MINUS_EQUALS: printf("TOK_MINUS_EQUALS"); break;
	case TOK_STAR_EQUALS: printf("TOK_STAR_EQUALS"); break;
	case TOK_SLASH_EQUALS: printf("TOK_SLASH_EQUALS"); break;
	case TOK_CARET_EQUALS: printf("TOK_CARET_EQUALS"); break;
	case TOK_PERCENT_EQUALS: printf("TOK_PERCENT_EQUALS"); break;
	case TOK_RANGE: printf("TOK_RANGE"); break;
	case TOK_MODULE: printf("TOK_MODULE"); break;
	case TOK_EOF: printf("TOK_EOF"); break;
	}
	if (token.value) printf(" \033[34m%s", token.value);
	printf("\033[0m\n");
}

int main(int argc, char** argv) {
	if (argc != 2) {
		fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
		return 1;
	}

	// Read the file
	FILE* file = fopen(argv[1], "r");
	if (!file) { perror("Error opening file"); return 1; }
	fseek(file, 0, SEEK_END);
	long size = ftell(file);
	fseek(file, 0, SEEK_SET);

	char* input = malloc(size + 1);
	fread(input, 1, size, file);
	input[size] = '\0';
	fclose(file);

	// Tokenise and print
	Token* tokens = lex(input);
	for (int i = 0; tokens[i].type != TOK_EOF; i++) {
		print_token(tokens[i]);
	}

	free_tokens(tokens);
	return 0;
}
