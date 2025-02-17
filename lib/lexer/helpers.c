#include "helpers.h"
#include <string.h>

TokenType get_word_type(const char* word) {
// GENERATE BEGIN WORD {{{
	if (strcmp(word, "if") == 0) return TOK_WORD_IF;
	if (strcmp(word, "then") == 0) return TOK_WORD_THEN;
	if (strcmp(word, "else") == 0) return TOK_WORD_ELSE;
	if (strcmp(word, "elif") == 0) return TOK_WORD_ELIF;
	if (strcmp(word, "case") == 0) return TOK_WORD_CASE;
	if (strcmp(word, "of") == 0) return TOK_WORD_OF;
	if (strcmp(word, "end") == 0) return TOK_WORD_END;
	if (strcmp(word, "while") == 0) return TOK_WORD_WHILE;
	if (strcmp(word, "for") == 0) return TOK_WORD_FOR;
	if (strcmp(word, "in") == 0) return TOK_WORD_IN;
	if (strcmp(word, "do") == 0) return TOK_WORD_DO;
	if (strcmp(word, "done") == 0) return TOK_WORD_DONE;
	if (strcmp(word, "break") == 0) return TOK_WORD_BREAK;
	if (strcmp(word, "continue") == 0) return TOK_WORD_CONTINUE;
	if (strcmp(word, "match") == 0) return TOK_WORD_MATCH;
	if (strcmp(word, "when") == 0) return TOK_WORD_WHEN;
	if (strcmp(word, "type") == 0) return TOK_WORD_TYPE;
	if (strcmp(word, "record") == 0) return TOK_WORD_RECORD;
	if (strcmp(word, "union") == 0) return TOK_WORD_UNION;
	if (strcmp(word, "fn") == 0) return TOK_WORD_FN;
	if (strcmp(word, "let") == 0) return TOK_WORD_LET;
	if (strcmp(word, "import") == 0) return TOK_WORD_IMPORT;
	if (strcmp(word, "use") == 0) return TOK_WORD_USE;
	if (strcmp(word, "and") == 0) return TOK_WORD_AND;
	if (strcmp(word, "or") == 0) return TOK_WORD_OR;
	if (strcmp(word, "xor") == 0) return TOK_WORD_XOR;
	if (strcmp(word, "true") == 0) return TOK_LIT_TRUE;
	if (strcmp(word, "false") == 0) return TOK_LIT_FALSE;
// GENERATE END WORD }}}
	return TOK_IDENT;
}

TokenType get_symbol_type(char c1, char c2) {
	switch (c1) {
// GENERATE BEGIN SYMBOL {{{
	case '(': return TOK_PAREN_L;
	case ')': return TOK_PAREN_R;
	case '[': return TOK_BRACKET_L;
	case ']': return TOK_BRACKET_R;
	case '{': return TOK_BRACE_L;
	case '}': return TOK_BRACE_R;
	case '.':
		switch (c2) {
		case '.': return TOK_RANGE;
		default: return TOK_DOT;
		}
	case ',': return TOK_COMMA;
	case ';': return TOK_SEMICOLON;
	case ':':
		switch (c2) {
		case ':': return TOK_MODULE;
		default: return TOK_COLON;
		}
	case '=':
		switch (c2) {
		case '=': return TOK_COMP_EQ;
		case '>': return TOK_RETURN_ARROW;
		default: return TOK_EQUALS;
		}
	case '|': return TOK_VERT_LINE;
	case '!':
		switch (c2) {
		case '=': return TOK_COMP_NE;
		default: return TOK_BANG;
		}
	case '-':
		switch (c2) {
		case '>': return TOK_ARROW;
		default: return TOK_MINUS;
		}
	case '+':
		switch (c2) {
		case '+': return TOK_CONCAT;
		default: return TOK_PLUS;
		}
	case '*': return TOK_STAR;
	case '/': return TOK_SLASH;
	case '%': return TOK_PERCENT;
	case '<':
		switch (c2) {
		case '=': return TOK_COMP_LE;
		default: return TOK_LESS;
		}
	case '>':
		switch (c2) {
		case '=': return TOK_COMP_GE;
		case '>': return TOK_FWD_COMPOSE;
		default: return TOK_GREATER;
		}
// GENERATE END SYMBOL }}}
	default: return TOK_ERR;
	}
}

bool is_long_symbol(TokenType type) {
	switch (type) {
// GENERATE BEGIN ISLONG {{{
	case TOK_PAREN_L: return false;
	case TOK_PAREN_R: return false;
	case TOK_BRACKET_L: return false;
	case TOK_BRACKET_R: return false;
	case TOK_BRACE_L: return false;
	case TOK_BRACE_R: return false;
	case TOK_DOT: return false;
	case TOK_COMMA: return false;
	case TOK_SEMICOLON: return false;
	case TOK_COLON: return false;
	case TOK_EQUALS: return false;
	case TOK_VERT_LINE: return false;
	case TOK_BANG: return false;
	case TOK_MINUS: return false;
	case TOK_PLUS: return false;
	case TOK_STAR: return false;
	case TOK_SLASH: return false;
	case TOK_PERCENT: return false;
	case TOK_LESS: return false;
	case TOK_GREATER: return false;
// GENERATE END ISLONG }}}
	case TOK_ERR: return false;
	default: return true;
	}
}
