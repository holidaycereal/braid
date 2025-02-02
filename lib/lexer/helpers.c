#include "helpers.h"
#include <string.h>

TokenType get_word_type(const char* word) {
// GENERATE BEGIN WORD {{{
	if (strcmp(word, "if") == 0) return TOK_WORD_IF;
	if (strcmp(word, "else") == 0) return TOK_WORD_ELSE;
	if (strcmp(word, "elif") == 0) return TOK_WORD_ELIF;
	if (strcmp(word, "while") == 0) return TOK_WORD_WHILE;
	if (strcmp(word, "for") == 0) return TOK_WORD_FOR;
	if (strcmp(word, "in") == 0) return TOK_WORD_IN;
	if (strcmp(word, "to") == 0) return TOK_WORD_TO;
	if (strcmp(word, "break") == 0) return TOK_WORD_BREAK;
	if (strcmp(word, "continue") == 0) return TOK_WORD_CONTINUE;
	if (strcmp(word, "match") == 0) return TOK_WORD_MATCH;
	if (strcmp(word, "when") == 0) return TOK_WORD_WHEN;
	if (strcmp(word, "switch") == 0) return TOK_WORD_SWITCH;
	if (strcmp(word, "case") == 0) return TOK_WORD_CASE;
	if (strcmp(word, "default") == 0) return TOK_WORD_DEFAULT;
	if (strcmp(word, "immut") == 0) return TOK_WORD_IMMUT;
	if (strcmp(word, "type") == 0) return TOK_WORD_TYPE;
	if (strcmp(word, "alias") == 0) return TOK_WORD_ALIAS;
	if (strcmp(word, "record") == 0) return TOK_WORD_RECORD;
	if (strcmp(word, "union") == 0) return TOK_WORD_UNION;
	if (strcmp(word, "fn") == 0) return TOK_WORD_FN;
	if (strcmp(word, "import") == 0) return TOK_WORD_IMPORT;
	if (strcmp(word, "as") == 0) return TOK_WORD_AS;
	if (strcmp(word, "from") == 0) return TOK_WORD_FROM;
	if (strcmp(word, "and") == 0) return TOK_WORD_AND;
	if (strcmp(word, "or") == 0) return TOK_WORD_OR;
	if (strcmp(word, "xor") == 0) return TOK_WORD_XOR;
	if (strcmp(word, "not") == 0) return TOK_WORD_NOT;
	if (strcmp(word, "u8") == 0) return TOK_PRIM_U8;
	if (strcmp(word, "u16") == 0) return TOK_PRIM_U16;
	if (strcmp(word, "u32") == 0) return TOK_PRIM_U32;
	if (strcmp(word, "u64") == 0) return TOK_PRIM_U64;
	if (strcmp(word, "i8") == 0) return TOK_PRIM_I8;
	if (strcmp(word, "i16") == 0) return TOK_PRIM_I16;
	if (strcmp(word, "i32") == 0) return TOK_PRIM_I32;
	if (strcmp(word, "i64") == 0) return TOK_PRIM_I64;
	if (strcmp(word, "uint") == 0) return TOK_PRIM_UINT;
	if (strcmp(word, "int") == 0) return TOK_PRIM_INT;
	if (strcmp(word, "usize") == 0) return TOK_PRIM_USIZE;
	if (strcmp(word, "isize") == 0) return TOK_PRIM_ISIZE;
	if (strcmp(word, "f32") == 0) return TOK_PRIM_F32;
	if (strcmp(word, "f64") == 0) return TOK_PRIM_F64;
	if (strcmp(word, "float") == 0) return TOK_PRIM_FLOAT;
	if (strcmp(word, "bool") == 0) return TOK_PRIM_BOOL;
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
		case '<': return TOK_BIT_LSL;
		case '>': return TOK_BIT_LSR;
		case '*': return TOK_BIT_AND;
		case '+': return TOK_BIT_OR;
		case '^': return TOK_BIT_XOR;
		default: return TOK_DOT;
		}
	case ',': return TOK_COMMA;
	case ';': return TOK_SEMICOLON;
	case ':':
		switch (c2) {
		case ':': return TOK_MODULE;
		case '<': return TOK_BIT_ASL;
		case '>': return TOK_BIT_ASR;
		default: return TOK_COLON;
		}
	case '=':
		switch (c2) {
		case '=': return TOK_COMP_EQ;
		case '>': return TOK_RETURN_ARROW;
		default: return TOK_EQUALS;
		}
	case '|': return TOK_VERT_LINE;
	case '&': return TOK_AMPERSAND;
	case '~': return TOK_TILDE;
	case '!':
		switch (c2) {
		case '=': return TOK_COMP_NE;
		default: return TOK_BANG;
		}
	case '?': return TOK_QUESTION;
	case '^': return TOK_CARET;
	case '-':
		switch (c2) {
		case '=': return TOK_SUB_ASSIGN;
		case '>': return TOK_ARROW;
		default: return TOK_MINUS;
		}
	case '+':
		switch (c2) {
		case '=': return TOK_ADD_ASSIGN;
		case '+': return TOK_CONCAT;
		default: return TOK_PLUS;
		}
	case '*':
		switch (c2) {
		case '=': return TOK_MUL_ASSIGN;
		default: return TOK_STAR;
		}
	case '/':
		switch (c2) {
		case '=': return TOK_DIV_ASSIGN;
		default: return TOK_SLASH;
		}
	case '%':
		switch (c2) {
		case '=': return TOK_MOD_ASSIGN;
		default: return TOK_PERCENT;
		}
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
	case TOK_AMPERSAND: return false;
	case TOK_TILDE: return false;
	case TOK_BANG: return false;
	case TOK_QUESTION: return false;
	case TOK_CARET: return false;
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
