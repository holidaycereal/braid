#include "lexer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char* token_type_to_string(TokenType type) {
	switch (type) {
	case WORD_IF: return "WORD_IF"; break;
	case WORD_THEN: return "WORD_THEN"; break;
	case WORD_ELSE: return "WORD_ELSE"; break;
	case WORD_WHILE: return "WORD_WHILE"; break;
	case WORD_FOR: return "WORD_FOR"; break;
	case WORD_IN: return "WORD_IN"; break;
	case WORD_DO: return "WORD_DO"; break;
	case WORD_OF: return "WORD_OF"; break;
	case WORD_BREAK: return "WORD_BREAK"; break;
	case WORD_CONTINUE: return "WORD_CONTINUE"; break;
	case WORD_MATCH: return "WORD_MATCH"; break;
	case WORD_TEST: return "WORD_TEST"; break;
	case WORD_DYN: return "WORD_DYN"; break;
	case WORD_PROC: return "WORD_PROC"; break;
	case WORD_DATA: return "WORD_DATA"; break;
	case WORD_TYPE: return "WORD_TYPE"; break;
	case WORD_FROM: return "WORD_FROM"; break;
	case WORD_TRAIT: return "WORD_TRAIT"; break;
	case WORD_BASE: return "WORD_BASE"; break;
	case WORD_IMPL: return "WORD_IMPL"; break;
	case WORD_SELF: return "WORD_SELF"; break;
	case WORD_INCLUDE: return "WORD_INCLUDE"; break;
	case WORD_IMPORT: return "WORD_IMPORT"; break;
	case WORD_ALIAS: return "WORD_ALIAS"; break;
	case WORD_EXIT: return "WORD_EXIT"; break;

	case PRIM_U8: return "PRIM_U8"; break;
	case PRIM_U16: return "PRIM_U16"; break;
	case PRIM_U32: return "PRIM_U32"; break;
	case PRIM_U64: return "PRIM_U64"; break;
	case PRIM_I8: return "PRIM_I8"; break;
	case PRIM_I16: return "PRIM_I16"; break;
	case PRIM_I32: return "PRIM_I32"; break;
	case PRIM_I64: return "PRIM_I64"; break;
	case PRIM_USIZE: return "PRIM_USIZE"; break;
	case PRIM_ISIZE: return "PRIM_ISIZE"; break;
	case PRIM_F32: return "PRIM_F32"; break;
	case PRIM_F64: return "PRIM_F64"; break;
	case PRIM_BOOL: return "PRIM_BOOL"; break;

	case LIT_TRUE: return "LIT_TRUE"; break;
	case LIT_FALSE: return "LIT_FALSE"; break;
	case LSL: return "LSL"; break;
	case LSR: return "LSR"; break;
	case ASL: return "ASL"; break;
	case ASR: return "ASR"; break;

	case IDENT: return "IDENT"; break;
	case LIT_NUM: return "LIT_NUM"; break;
	case LIT_CHAR: return "LIT_CHAR"; break;
	case LIT_STR: return "LIT_STR"; break;
	case LIT_STR_RAW: return "LIT_STR_RAW"; break;
	case ERR_TOKEN: return "ERR_TOKEN"; break;

	case PAREN_L: return "PAREN_L"; break;
	case PAREN_R: return "PAREN_R"; break;
	case BRACKET_L: return "BRACKET_L"; break;
	case BRACKET_R: return "BRACKET_R"; break;
	case BRACE_L: return "BRACE_L"; break;
	case BRACE_R: return "BRACE_R"; break;
	case DOT: return "DOT"; break;
	case COMMA: return "COMMA"; break;
	case SEMICOLON: return "SEMICOLON"; break;
	case COLON: return "COLON"; break;
	case EQUALS: return "EQUALS"; break;
	case VERT_LINE: return "VERT_LINE"; break;
	case DOLLAR: return "DOLLAR"; break;
	case AMPERSAND: return "AMPERSAND"; break;
	case BANG: return "BANG"; break;
	case QUESTION: return "QUESTION"; break;
	case HASH: return "HASH"; break;
	case CARET: return "CARET"; break;
	case MINUS: return "MINUS"; break;
	case PLUS: return "PLUS"; break;
	case STAR: return "STAR"; break;
	case SLASH: return "SLASH"; break;
	case PERCENT: return "PERCENT"; break;
	case LESS: return "LESS"; break;
	case GREATER: return "GREATER"; break;

	case TEST_EQ: return "TEST_EQ"; break;
	case NOT_EQ: return "NOT_EQ"; break;
	case LESS_EQ: return "LESS_EQ"; break;
	case GREATER_EQ: return "GREATER_EQ"; break;
	case AND: return "AND"; break;
	case OR: return "OR"; break;
	case XOR: return "XOR"; break;
	case ARROW: return "ARROW"; break;
	case MUT_ARROW: return "MUT_ARROW"; break;
	case INCREMENT: return "INCREMENT"; break;
	case DECREMENT: return "DECREMENT"; break;
	case PLUS_EQUALS: return "PLUS_EQUALS"; break;
	case MINUS_EQUALS: return "MINUS_EQUALS"; break;
	case STAR_EQUALS: return "STAR_EQUALS"; break;
	case SLASH_EQUALS: return "SLASH_EQUALS"; break;
	case CARET_EQUALS: return "CARET_EQUALS"; break;
	case RANGE: return "RANGE"; break;
	case MODULE: return "MODULE"; break;

	case EOF_TOKEN: return "EOF_TOKEN"; break;
	default: return "Unknown"; break;
	}
}

int main(int argc, char** argv) {
	if (argc != 2) {
		fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
		return 1;
	}

	FILE* file = fopen(argv[1], "r");
	if (!file) {
		perror("Error opening file");
		return 1;
	}

	// Read the file
	fseek(file, 0, SEEK_END);
	long size = ftell(file);
	fseek(file, 0, SEEK_SET);

	char* buffer = malloc(size + 1);
	fread(buffer, 1, size, file);
	buffer[size] = '\0';
	fclose(file);

	// Do the lexing
	Token* tokens = lex(buffer);

	// Print the tokens
	int i = 0;
	while (tokens[i].type != EOF_TOKEN) {
		if (tokens[i].value) {
			printf("%s: %s\n",
					token_type_to_string(tokens[i].type), tokens[i].value);
		} else {
			printf("%s\n", token_type_to_string(tokens[i].type));
		}

		free((void*) tokens[i].value);
		i++;
	}
	printf("%s\n", token_type_to_string(tokens[i].type));

	free(buffer);
	free(tokens);

	return 0;
}

