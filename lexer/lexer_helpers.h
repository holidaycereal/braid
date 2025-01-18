#ifndef LEXER_HELPERS_H
#define LEXER_HELPERS_H

#include "lexer.h"

TokenType get_word_type(const char* word);

TokenType get_symbol_type(char c1, char c2);

#endif
