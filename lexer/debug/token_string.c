#include "token_string.h"

const char* token_type_to_string(TokenType type) {
	switch (type) {
	case TOK_WORD_IF: return "TOK_WORD_IF";
	case TOK_WORD_THEN: return "TOK_WORD_THEN";
	case TOK_WORD_ELSE: return "TOK_WORD_ELSE";
	case TOK_WORD_WHILE: return "TOK_WORD_WHILE";
	case TOK_WORD_FOR: return "TOK_WORD_FOR";
	case TOK_WORD_IN: return "TOK_WORD_IN";
	case TOK_WORD_DO: return "TOK_WORD_DO";
	case TOK_WORD_BREAK: return "TOK_WORD_BREAK";
	case TOK_WORD_CONTINUE: return "TOK_WORD_CONTINUE";
	case TOK_WORD_MATCH: return "TOK_WORD_MATCH";
	case TOK_WORD_WITH: return "TOK_WORD_WITH";
	case TOK_WORD_DYN: return "TOK_WORD_DYN";
	case TOK_WORD_TYPE: return "TOK_WORD_TYPE";
	case TOK_WORD_RECORD: return "TOK_WORD_RECORD";
	case TOK_WORD_INCLUDE: return "TOK_WORD_INCLUDE";
	case TOK_WORD_IMPORT: return "TOK_WORD_IMPORT";
	case TOK_WORD_EXIT: return "TOK_WORD_EXIT";
	case TOK_WORD_AND: return "TOK_WORD_AND";
	case TOK_WORD_OR: return "TOK_WORD_OR";
	case TOK_WORD_XOR: return "TOK_WORD_XOR";

	case TOK_PRIM_U8: return "TOK_PRIM_U8";
	case TOK_PRIM_U16: return "TOK_PRIM_U16";
	case TOK_PRIM_U32: return "TOK_PRIM_U32";
	case TOK_PRIM_U64: return "TOK_PRIM_U64";
	case TOK_PRIM_I8: return "TOK_PRIM_I8";
	case TOK_PRIM_I16: return "TOK_PRIM_I16";
	case TOK_PRIM_I32: return "TOK_PRIM_I32";
	case TOK_PRIM_I64: return "TOK_PRIM_I64";
	case TOK_PRIM_USIZE: return "TOK_PRIM_USIZE";
	case TOK_PRIM_ISIZE: return "TOK_PRIM_ISIZE";
	case TOK_PRIM_F32: return "TOK_PRIM_F32";
	case TOK_PRIM_F64: return "TOK_PRIM_F64";
	case TOK_PRIM_BOOL: return "TOK_PRIM_BOOL";

	case TOK_LIT_TRUE: return "TOK_LIT_TRUE";
	case TOK_LIT_FALSE: return "TOK_LIT_FALSE";

	case TOK_IDENT: return "TOK_IDENT";
	case TOK_LIT_INT_DEC: return "TOK_LIT_INT_DEC";
	case TOK_LIT_INT_HEX: return "TOK_LIT_INT_HEX";
	case TOK_LIT_INT_OCT: return "TOK_LIT_INT_OCT";
	case TOK_LIT_INT_BIN: return "TOK_LIT_INT_BIN";
	case TOK_LIT_FLOAT: return "TOK_LIT_FLOAT";
	case TOK_LIT_CHAR: return "TOK_LIT_CHAR";
	case TOK_LIT_STR: return "TOK_LIT_STR";
	case TOK_LIT_STR_RAW: return "TOK_LIT_STR_RAW";
	case TOK_ERR: return "TOK_ERR";

	case TOK_PAREN_L: return "TOK_PAREN_L";
	case TOK_PAREN_R: return "TOK_PAREN_R";
	case TOK_BRACKET_L: return "TOK_BRACKET_L";
	case TOK_BRACKET_R: return "TOK_BRACKET_R";
	case TOK_BRACE_L: return "TOK_BRACE_L";
	case TOK_BRACE_R: return "TOK_BRACE_R";
	case TOK_DOT: return "TOK_DOT";
	case TOK_COMMA: return "TOK_COMMA";
	case TOK_SEMICOLON: return "TOK_SEMICOLON";
	case TOK_COLON: return "TOK_COLON";
	case TOK_EQUALS: return "TOK_EQUALS";
	case TOK_VERT_LINE: return "TOK_VERT_LINE";
	case TOK_AMPERSAND: return "TOK_AMPERSAND";
	case TOK_BANG: return "TOK_BANG";
	case TOK_QUESTION: return "TOK_QUESTION";
	case TOK_CARET: return "TOK_CARET";
	case TOK_MINUS: return "TOK_MINUS";
	case TOK_PLUS: return "TOK_PLUS";
	case TOK_STAR: return "TOK_STAR";
	case TOK_SLASH: return "TOK_SLASH";
	case TOK_PERCENT: return "TOK_PERCENT";
	case TOK_LESS: return "TOK_LESS";
	case TOK_GREATER: return "TOK_GREATER";
	case TOK_COMP_EQ: return "TOK_COMP_EQ";
	case TOK_COMP_NE: return "TOK_COMP_NE";
	case TOK_COMP_LE: return "TOK_COMP_LE";
	case TOK_COMP_GE: return "TOK_COMP_GE";
	case TOK_ARROW: return "TOK_ARROW";
	case TOK_FWD_COMPOSE: return "TOK_FWD_COMPOSE";
	case TOK_PLUS_EQUALS: return "TOK_PLUS_EQUALS";
	case TOK_MINUS_EQUALS: return "TOK_MINUS_EQUALS";
	case TOK_STAR_EQUALS: return "TOK_STAR_EQUALS";
	case TOK_SLASH_EQUALS: return "TOK_SLASH_EQUALS";
	case TOK_CARET_EQUALS: return "TOK_CARET_EQUALS";
	case TOK_PERCENT_EQUALS: return "TOK_PERCENT_EQUALS";
	case TOK_RANGE: return "TOK_RANGE";
	case TOK_MODULE: return "TOK_MODULE";

	case TOK_EOF: return "TOK_EOF";

	default: return "unreachable";
	}
}
