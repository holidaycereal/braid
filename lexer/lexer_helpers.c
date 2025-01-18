#include "lexer_helpers.h"
#include <string.h>

TokenType get_word_type(const char* word) {
	if (strcmp(word, "if") == 0) return WORD_IF;
	if (strcmp(word, "then") == 0) return WORD_THEN;
	if (strcmp(word, "else") == 0) return WORD_ELSE;
	if (strcmp(word, "while") == 0) return WORD_WHILE;
	if (strcmp(word, "for") == 0) return WORD_FOR;
	if (strcmp(word, "in") == 0) return WORD_IN;
	if (strcmp(word, "break") == 0) return WORD_BREAK;
	if (strcmp(word, "continue") == 0) return WORD_CONTINUE;
	if (strcmp(word, "match") == 0) return WORD_MATCH;
	if (strcmp(word, "dyn") == 0) return WORD_DYN;
	if (strcmp(word, "data") == 0) return WORD_DATA;
	if (strcmp(word, "type") == 0) return WORD_TYPE;
	if (strcmp(word, "include") == 0) return WORD_INCLUDE;
	if (strcmp(word, "import") == 0) return WORD_IMPORT;
	if (strcmp(word, "exit") == 0) return WORD_EXIT;

	if (strcmp(word, "u8") == 0) return PRIM_U8;
	if (strcmp(word, "u16") == 0) return PRIM_U16;
	if (strcmp(word, "u32") == 0) return PRIM_U32;
	if (strcmp(word, "u64") == 0) return PRIM_U64;
	if (strcmp(word, "i8") == 0) return PRIM_I8;
	if (strcmp(word, "i16") == 0) return PRIM_I16;
	if (strcmp(word, "i32") == 0) return PRIM_I32;
	if (strcmp(word, "i64") == 0) return PRIM_I64;
	if (strcmp(word, "usize") == 0) return PRIM_USIZE;
	if (strcmp(word, "isize") == 0) return PRIM_ISIZE;
	if (strcmp(word, "f32") == 0) return PRIM_F32;
	if (strcmp(word, "f64") == 0) return PRIM_F64;
	if (strcmp(word, "bool") == 0) return PRIM_BOOL;

	if (strcmp(word, "true") == 0) return LIT_TRUE;
	if (strcmp(word, "false") == 0) return LIT_FALSE;

	return IDENT;
}

TokenType get_symbol_type(char c1, char c2) {
	return
	  c1 == '(' ? PAREN_L
	: c1 == ')' ? PAREN_R
	: c1 == '[' ? BRACKET_L
	: c1 == ']' ? BRACKET_R
	: c1 == '{' ? BRACE_L
	: c1 == '}' ? BRACE_R
	: c1 == '.' ? c2 == '.' ? RANGE : DOT
	: c1 == ',' ? COMMA
	: c1 == ';' ? SEMICOLON
	: c1 == ':' ? c2 == ':' ? MODULE : COLON
	: c1 == '=' ? c2 == '=' ? TEST_EQ : EQUALS
	: c1 == '|' ? c2 == '|' ? OR : VERT_LINE
	: c1 == '$' ? DOLLAR
	: c1 == '&' ? c2 == '&' ? AND : AMPERSAND
	: c1 == '!' ? c2 == '=' ? NOT_EQ : BANG
	: c1 == '?' ? QUESTION
	: c1 == '#' ? c2 == '#' ? XOR : HASH
	: c1 == '^' ? c2 == '=' ? CARET_EQUALS : CARET
	: c1 == '-' ? c2 == '=' ? MINUS_EQUALS : c2 == '>' ? ARROW : MINUS
	: c1 == '+' ? c2 == '=' ? PLUS_EQUALS : PLUS
	: c1 == '*' ? c2 == '=' ? STAR_EQUALS : STAR
	: c1 == '/' ? c2 == '=' ? SLASH_EQUALS : SLASH
	: c1 == '%' ? c2 == '=' ? PERCENT_EQUALS : PERCENT
	: c1 == '<' ? c2 == '=' ? LESS_EQ : LESS
	: c1 == '>' ? c2 == '=' ? GREATER_EQ : GREATER
	: ERR_TOKEN;
}

bool is_long_symbol(TokenType type) {
	return
	   type == RANGE
	|| type == MODULE
	|| type == TEST_EQ
	|| type == OR
	|| type == AND
	|| type == NOT_EQ
	|| type == XOR
	|| type == CARET_EQUALS
	|| type == MINUS_EQUALS
	|| type == ARROW
	|| type == PLUS_EQUALS
	|| type == STAR_EQUALS
	|| type == SLASH_EQUALS
	|| type == PERCENT_EQUALS
	|| type == LESS_EQ
	|| type == GREATER_EQ;
}
