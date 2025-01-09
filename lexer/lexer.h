#ifndef LEXER_H
#define LEXER_H

#include "token_type.h"

typedef struct {
	TokenType type;
	char* value;
} Token;

Token* make_token(TokenType type, const char* value);
Token* lex(const char* input);

#endif
