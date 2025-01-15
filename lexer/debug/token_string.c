#include "token_string.h"

const char* token_type_to_string(TokenType type) {
	switch (type) {
	case WORD_IF: return "WORD_IF";
	case WORD_THEN: return "WORD_THEN";
	case WORD_ELSE: return "WORD_ELSE";
	case WORD_WHILE: return "WORD_WHILE";
	case WORD_FOR: return "WORD_FOR";
	case WORD_IN: return "WORD_IN";
	case WORD_BREAK: return "WORD_BREAK";
	case WORD_CONTINUE: return "WORD_CONTINUE";
	case WORD_MATCH: return "WORD_MATCH";
	case WORD_DYN: return "WORD_DYN";
	case WORD_DATA: return "WORD_DATA";
	case WORD_TYPE: return "WORD_TYPE";
	case WORD_INCLUDE: return "WORD_INCLUDE";
	case WORD_IMPORT: return "WORD_IMPORT";
	case WORD_EXIT: return "WORD_EXIT";

	case PRIM_U8: return "PRIM_U8";
	case PRIM_U16: return "PRIM_U16";
	case PRIM_U32: return "PRIM_U32";
	case PRIM_U64: return "PRIM_U64";
	case PRIM_I8: return "PRIM_I8";
	case PRIM_I16: return "PRIM_I16";
	case PRIM_I32: return "PRIM_I32";
	case PRIM_I64: return "PRIM_I64";
	case PRIM_USIZE: return "PRIM_USIZE";
	case PRIM_ISIZE: return "PRIM_ISIZE";
	case PRIM_F32: return "PRIM_F32";
	case PRIM_F64: return "PRIM_F64";
	case PRIM_BOOL: return "PRIM_BOOL";

	case LIT_TRUE: return "LIT_TRUE";
	case LIT_FALSE: return "LIT_FALSE";

	case IDENT: return "IDENT";
	case LIT_NUM: return "LIT_NUM";
	case LIT_CHAR: return "LIT_CHAR";
	case LIT_STR: return "LIT_STR";
	case LIT_STR_RAW: return "LIT_STR_RAW";
	case ERR_TOKEN: return "ERR_TOKEN";

	case PAREN_L: return "PAREN_L";
	case PAREN_R: return "PAREN_R";
	case BRACKET_L: return "BRACKET_L";
	case BRACKET_R: return "BRACKET_R";
	case BRACE_L: return "BRACE_L";
	case BRACE_R: return "BRACE_R";
	case DOT: return "DOT";
	case COMMA: return "COMMA";
	case SEMICOLON: return "SEMICOLON";
	case COLON: return "COLON";
	case EQUALS: return "EQUALS";
	case VERT_LINE: return "VERT_LINE";
	case DOLLAR: return "DOLLAR";
	case AMPERSAND: return "AMPERSAND";
	case BANG: return "BANG";
	case QUESTION: return "QUESTION";
	case HASH: return "HASH";
	case CARET: return "CARET";
	case MINUS: return "MINUS";
	case PLUS: return "PLUS";
	case STAR: return "STAR";
	case SLASH: return "SLASH";
	case PERCENT: return "PERCENT";
	case LESS: return "LESS";
	case GREATER: return "GREATER";
	case TEST_EQ: return "TEST_EQ";
	case NOT_EQ: return "NOT_EQ";
	case LESS_EQ: return "LESS_EQ";
	case GREATER_EQ: return "GREATER_EQ";
	case AND: return "AND";
	case OR: return "OR";
	case XOR: return "XOR";
	case ARROW: return "ARROW";
	case PLUS_EQUALS: return "PLUS_EQUALS";
	case MINUS_EQUALS: return "MINUS_EQUALS";
	case STAR_EQUALS: return "STAR_EQUALS";
	case SLASH_EQUALS: return "SLASH_EQUALS";
	case CARET_EQUALS: return "CARET_EQUALS";
	case PERCENT_EQUALS: return "PERCENT_EQUALS";
	case RANGE: return "RANGE";
	case MODULE: return "MODULE";

	case EOF_TOKEN: return "EOF_TOKEN";

	default: return "unknown";
	}
}
