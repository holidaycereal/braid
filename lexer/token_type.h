#ifndef TOKEN_TYPE_H
#define TOKEN_TYPE_H

typedef enum {
	// Keywords
	WORD_IF,
	WORD_THEN,
	WORD_ELSE,
	WORD_WHILE,
	WORD_FOR,
	WORD_IN,
	WORD_BREAK,
	WORD_CONTINUE,
	WORD_MATCH,
	WORD_DYN,
	WORD_DATA,
	WORD_TYPE,
	WORD_INCLUDE,
	WORD_IMPORT,
	WORD_EXIT,

	// Primitives
	PRIM_U8,
	PRIM_U16,
	PRIM_U32,
	PRIM_U64,
	PRIM_I8,
	PRIM_I16,
	PRIM_I32,
	PRIM_I64,
	PRIM_USIZE,
	PRIM_ISIZE,
	PRIM_F32,
	PRIM_F64,
	PRIM_BOOL,

	// Boolean literals
	LIT_TRUE,
	LIT_FALSE,

	// Tokens with a string value
	IDENT,
	LIT_INT_DEC,
	LIT_INT_HEX,
	LIT_INT_OCT,
	LIT_INT_BIN,
	LIT_FLOAT,
	LIT_CHAR,
	LIT_STR,
	LIT_STR_RAW,
	ERR_TOKEN,

	// Punctuation & operators
	PAREN_L,
	PAREN_R,
	BRACKET_L,
	BRACKET_R,
	BRACE_L,
	BRACE_R,
	DOT,
	COMMA,
	SEMICOLON,
	COLON,
	EQUALS,
	VERT_LINE,
	DOLLAR,
	AMPERSAND,
	BANG,
	QUESTION,
	HASH,
	CARET,
	MINUS,
	PLUS,
	STAR,
	SLASH,
	PERCENT,
	LESS,
	GREATER,
	// Multi-character operators
	TEST_EQ,         //  ==
	NOT_EQ,          //  !=
	LESS_EQ,         //  <=
	GREATER_EQ,      //  >=
	AND,             //  &&
	OR,              //  ||
	XOR,             //  ##
	ARROW,           //  ->
	PLUS_EQUALS,     //  +=
	MINUS_EQUALS,    //  -=
	STAR_EQUALS,     //  *=
	SLASH_EQUALS,    //  /=
	CARET_EQUALS,    //  ^=
	PERCENT_EQUALS,  //  %=
	RANGE,           //  ..
	MODULE,          //  ::

	EOF_TOKEN,  // End of input
} TokenType;

#endif
