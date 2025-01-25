#ifndef HELPERS_H
#define HELPERS_H

#include "lexer.h"
#include <stdbool.h>

TokenType get_word_type(const char* word);
TokenType get_symbol_type(char c1, char c2);
bool is_long_symbol(TokenType type);

#endif
