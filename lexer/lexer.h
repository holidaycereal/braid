#ifndef LEXER_H
#define LEXER_H

#include <stddef.h>

typedef enum {
	TOK_WORD_IF,
	TOK_WORD_ELSE,
	TOK_WORD_ELIF,
	TOK_WORD_WHILE,
	TOK_WORD_FOR,
	TOK_WORD_IN,
	TOK_WORD_BREAK,
	TOK_WORD_CONTINUE,
	TOK_WORD_MATCH,
	TOK_WORD_WITH,
	TOK_WORD_WHEN,
	TOK_WORD_SWITCH,
	TOK_WORD_DYN,
	TOK_WORD_TYPE,
	TOK_WORD_RECORD,
	TOK_WORD_INCLUDE,
	TOK_WORD_IMPORT,
	TOK_WORD_EXIT,
	TOK_WORD_AND,
	TOK_WORD_OR,
	TOK_WORD_XOR,
	TOK_WORD_NOT,
	TOK_PRIM_U8,
	TOK_PRIM_U16,
	TOK_PRIM_U32,
	TOK_PRIM_U64,
	TOK_PRIM_I8,
	TOK_PRIM_I16,
	TOK_PRIM_I32,
	TOK_PRIM_I64,
	TOK_PRIM_USIZE,
	TOK_PRIM_ISIZE,
	TOK_PRIM_F32,
	TOK_PRIM_F64,
	TOK_PRIM_BOOL,
	TOK_LIT_TRUE,
	TOK_LIT_FALSE,
	TOK_IDENT,
	TOK_LIT_INT_DEC,
	TOK_LIT_INT_HEX,
	TOK_LIT_INT_OCT,
	TOK_LIT_INT_BIN,
	TOK_LIT_FLOAT,
	TOK_LIT_CHAR,
	TOK_LIT_STR,
	TOK_LIT_STR_RAW,
	TOK_ERR,
	TOK_PAREN_L,
	TOK_PAREN_R,
	TOK_BRACKET_L,
	TOK_BRACKET_R,
	TOK_BRACE_L,
	TOK_BRACE_R,
	TOK_DOT,
	TOK_COMMA,
	TOK_SEMICOLON,
	TOK_COLON,
	TOK_EQUALS,
	TOK_VERT_LINE,
	TOK_AMPERSAND,
	TOK_BANG,
	TOK_QUESTION,
	TOK_CARET,
	TOK_MINUS,
	TOK_PLUS,
	TOK_STAR,
	TOK_SLASH,
	TOK_PERCENT,
	TOK_LESS,
	TOK_GREATER,
	TOK_COMP_EQ,         //  ==
	TOK_COMP_NE,         //  !=
	TOK_COMP_LE,         //  <=
	TOK_COMP_GE,         //  >=
	TOK_ARROW,           //  ->
	TOK_RETURN_ARROW,    //  =>
	TOK_FWD_COMPOSE,     //  >>
	TOK_PLUS_EQUALS,     //  +=
	TOK_MINUS_EQUALS,    //  -=
	TOK_STAR_EQUALS,     //  *=
	TOK_SLASH_EQUALS,    //  /=
	TOK_CARET_EQUALS,    //  ^=
	TOK_PERCENT_EQUALS,  //  %=
	TOK_RANGE,           //  ..
	TOK_MODULE,          //  ::
	TOK_EOF,
} TokenType;

typedef struct {
	TokenType type;
	char* value;
} Token;

Token make_token(TokenType type, const char* value);
Token* lex(const char* input);
void free_tokens(Token* tokens);

// For FFI
size_t get_token_size();
size_t get_token_alignment();
size_t get_token_type_size();

#endif
