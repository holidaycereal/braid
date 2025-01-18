#include "lexer_helpers.h"
#include <string.h>
#include <stdbool.h>

TokenType get_word_type(const char* word) {
	if (strcmp(word, "if") == 0) return TOK_WORD_IF;
	if (strcmp(word, "then") == 0) return TOK_WORD_THEN;
	if (strcmp(word, "else") == 0) return TOK_WORD_ELSE;
	if (strcmp(word, "while") == 0) return TOK_WORD_WHILE;
	if (strcmp(word, "for") == 0) return TOK_WORD_FOR;
	if (strcmp(word, "in") == 0) return TOK_WORD_IN;
	if (strcmp(word, "break") == 0) return TOK_WORD_BREAK;
	if (strcmp(word, "continue") == 0) return TOK_WORD_CONTINUE;
	if (strcmp(word, "match") == 0) return TOK_WORD_MATCH;
	if (strcmp(word, "dyn") == 0) return TOK_WORD_DYN;
	if (strcmp(word, "data") == 0) return TOK_WORD_DATA;
	if (strcmp(word, "type") == 0) return TOK_WORD_TYPE;
	if (strcmp(word, "include") == 0) return TOK_WORD_INCLUDE;
	if (strcmp(word, "import") == 0) return TOK_WORD_IMPORT;
	if (strcmp(word, "exit") == 0) return TOK_WORD_EXIT;

	if (strcmp(word, "u8") == 0) return TOK_PRIM_U8;
	if (strcmp(word, "u16") == 0) return TOK_PRIM_U16;
	if (strcmp(word, "u32") == 0) return TOK_PRIM_U32;
	if (strcmp(word, "u64") == 0) return TOK_PRIM_U64;
	if (strcmp(word, "i8") == 0) return TOK_PRIM_I8;
	if (strcmp(word, "i16") == 0) return TOK_PRIM_I16;
	if (strcmp(word, "i32") == 0) return TOK_PRIM_I32;
	if (strcmp(word, "i64") == 0) return TOK_PRIM_I64;
	if (strcmp(word, "usize") == 0) return TOK_PRIM_USIZE;
	if (strcmp(word, "isize") == 0) return TOK_PRIM_ISIZE;
	if (strcmp(word, "f32") == 0) return TOK_PRIM_F32;
	if (strcmp(word, "f64") == 0) return TOK_PRIM_F64;
	if (strcmp(word, "bool") == 0) return TOK_PRIM_BOOL;

	if (strcmp(word, "true") == 0) return TOK_LIT_TRUE;
	if (strcmp(word, "false") == 0) return TOK_LIT_FALSE;

	return TOK_IDENT;
}

TokenType get_symbol_type(char c1, char c2) {
	return
	  c1 == '(' ? TOK_PAREN_L
	: c1 == ')' ? TOK_PAREN_R
	: c1 == '[' ? TOK_BRACKET_L
	: c1 == ']' ? TOK_BRACKET_R
	: c1 == '{' ? TOK_BRACE_L
	: c1 == '}' ? TOK_BRACE_R
	: c1 == '.' ? c2 == '.' ? TOK_RANGE : TOK_DOT
	: c1 == ',' ? TOK_COMMA
	: c1 == ';' ? TOK_SEMICOLON
	: c1 == ':' ? c2 == ':' ? TOK_MODULE : TOK_COLON
	: c1 == '=' ? c2 == '=' ? TOK_COMP_EQ : TOK_EQUALS
	: c1 == '|' ? c2 == '|' ? TOK_OR : TOK_VERT_LINE
	: c1 == '$' ? TOK_DOLLAR
	: c1 == '&' ? c2 == '&' ? TOK_AND : TOK_AMPERSAND
	: c1 == '!' ? c2 == '=' ? TOK_COMP_NE : TOK_BANG
	: c1 == '?' ? TOK_QUESTION
	: c1 == '#' ? c2 == '#' ? TOK_XOR : TOK_HASH
	: c1 == '^' ? c2 == '=' ? TOK_CARET_EQUALS : TOK_CARET
	: c1 == '-' ? c2 == '=' ? TOK_MINUS_EQUALS : c2 == '>' ? TOK_ARROW : TOK_MINUS
	: c1 == '+' ? c2 == '=' ? TOK_PLUS_EQUALS : TOK_PLUS
	: c1 == '*' ? c2 == '=' ? TOK_STAR_EQUALS : TOK_STAR
	: c1 == '/' ? c2 == '=' ? TOK_SLASH_EQUALS : TOK_SLASH
	: c1 == '%' ? c2 == '=' ? TOK_PERCENT_EQUALS : TOK_PERCENT
	: c1 == '<' ? c2 == '=' ? TOK_COMP_LE : TOK_LESS
	: c1 == '>' ? c2 == '=' ? TOK_COMP_GE : c2 == '>' ? TOK_FWD_COMPOSE : TOK_GREATER
	: TOK_ERR;
}

bool is_long_symbol(TokenType type) {
	return
	   type == TOK_RANGE
	|| type == TOK_MODULE
	|| type == TOK_COMP_EQ
	|| type == TOK_OR
	|| type == TOK_AND
	|| type == TOK_COMP_NE
	|| type == TOK_XOR
	|| type == TOK_CARET_EQUALS
	|| type == TOK_MINUS_EQUALS
	|| type == TOK_ARROW
	|| type == TOK_FWD_COMPOSE
	|| type == TOK_PLUS_EQUALS
	|| type == TOK_STAR_EQUALS
	|| type == TOK_SLASH_EQUALS
	|| type == TOK_PERCENT_EQUALS
	|| type == TOK_COMP_LE
	|| type == TOK_COMP_GE;
}
