#include "lexer.h"
#include "helpers.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

#define CUR      input[pos]
#define NEXT     input[pos + 1]
#define NEXTNEXT input[pos + 2]
#define PREV     input[pos - 1]

Token make_token(TokenType type, const char* value) {
	return (Token) {
		.type = type,
		.value = value ? strdup(value) : NULL
	};
}

Token* lex(const char* input) {
	Token* tokens = malloc(sizeof(Token) * 128);
	size_t capacity = 128;
	size_t count = 0;  // Token count
	size_t pos = 0;    // Character position in input file

	while (CUR != '\0') {
		// Skip whitespace
		if (isspace(CUR)) {
			pos++;
			continue;
		}
		// Skip one-line comments
		if (CUR == '-' && NEXT == '-') {
			pos += 2;
			while (CUR != '\0' && CUR != '\n') pos++;
			continue;
		}
		// Skip block comments
		if (CUR == '-' && NEXT == '*') {
			pos += 2;
			size_t depth = 1;
			while (CUR != '\0') {
				if (CUR == '-' && NEXT == '*') {
					pos += 2;
					depth++;
				} else if (CUR == '*' && NEXT == '-') {
					pos += 2;
					depth--;
					if (depth == 0) break;
				} else {
					pos++;
				}
			}
			continue;
		}

		// Identifiers & builtin words
		if (isalpha(CUR) || CUR == '_') {
			size_t start = pos;
			while (isalnum(CUR) || CUR == '_') pos++;

			size_t len = pos - start;
			char* word = malloc(len + 1);
			strncpy(word, input + start, len);
			word[len] = '\0';

			TokenType type = get_word_type(word);
			tokens[count++] = make_token(type, type == TOK_IDENT ? word : NULL);
			free(word);
		}

		// Numeric literals
		// TODO: scientific notation
		else if (isdigit(CUR)) {
			size_t start = pos;
			bool has_dot = false;
			TokenType type = TOK_LIT_INT;

			// Hexadecimal
			if (CUR == '0' && (NEXT == 'x' || NEXT == 'X')) {
				pos += 2;
				while (isxdigit(CUR)) pos++;
			} // Octal
			else if (CUR == '0' && (NEXT == 'o' || NEXT == 'O')) {
				pos += 2;
				while (CUR >= '0' && CUR <= '7') pos++;
			} // Binary
			else if (CUR == '0' && (NEXT == 'b' || NEXT == 'B')) {
				pos += 2;
				while (CUR == '0' || CUR == '1') pos++;
			} // Decimal
			else {
				while (isdigit(CUR) ||
						(CUR == '.' && !has_dot && isdigit(NEXT))) {
					if (CUR == '.') has_dot = true;
					pos++;
				}
				if (has_dot) type = TOK_LIT_FLOAT;
			}

			size_t len = pos - start;
			char* num = malloc(len + 1);
			strncpy(num, input + start, len);
			num[len] = '\0';

			tokens[count++] = make_token(type, num);
			free(num);
		}

		// String literals
		else if (CUR == '"' || CUR == '`') {
			pos++;  // Skip opening quote
			size_t start = pos;
			TokenType type = PREV == '"' ? TOK_LIT_STR : TOK_LIT_STR_RAW;

			while (CUR != '\0' && CUR != (type == TOK_LIT_STR ? '"' : '`')) {
				if (CUR == '\\') pos++;
				if (CUR == '\0') break;
				pos++;
			}
			if (CUR == '\0') {
				tokens[count++] = make_token(TOK_ERR, "Unterminated string literal");
				continue;
			}

			size_t len = pos - start;
			char* str = malloc(len + 1);
			strncpy(str, input + start, len);
			str[len] = '\0';

			tokens[count++] = make_token(type, str);
			free(str);
			pos++;  // Skip closing quote
		}

		// Character literals
		else if (CUR == '\'') {
			pos++;  // Skip opening quote
			char ch[3];

			if (CUR != '\0' && NEXT != '\0') {
				if (CUR == '\\' && NEXTNEXT == '\'') {
					ch[0] = '\\';
					ch[1] = NEXT;
					ch[2] = '\0';
					pos += 2;
				} else if (CUR != '\'' && NEXT == '\'') {
					ch[0] = CUR;
					ch[1] = '\0';
					ch[2] = '\0';
					pos++;
				} else {
					while (CUR != '\0' && CUR != '\'') pos++;
					if (CUR == '\'') pos++;
					tokens[count++] = make_token(TOK_ERR, "Invalid character literal");
					continue;
				}
			} else {
				tokens[count++] = make_token(TOK_ERR, "Unterminated character literal");
				if (CUR != '\0') pos++;
				continue;
			}

			tokens[count++] = make_token(TOK_LIT_CHAR, ch);
			pos++;  // Skip closing quote
		}

		// Punctuation & operators
		else {
			TokenType type = get_symbol_type(CUR, NEXT);
			if (is_long_symbol(type)) pos++;
			pos++;
			if (type == TOK_ERR) {
				char err_str[32];
				sprintf(err_str, "Unknown character %c", PREV);
				tokens[count++] = make_token(TOK_ERR, err_str);
			} else {
				tokens[count++] = make_token(type, NULL);
			}
		}

		// Resize the token array if needed
		if (count >= capacity) {
			capacity *= 2;
			tokens = realloc(tokens, sizeof(Token) * capacity);
		}
	}

	// Safely add EOF token
	if (count >= capacity) {
		capacity++;
		tokens = realloc(tokens, sizeof(Token) * capacity);
	}
	tokens[count++] = make_token(TOK_EOF, NULL);

	return tokens;
}

void free_tokens(Token* tokens) {
	if (!tokens) return;
	for (int i = 0; tokens[i].type != TOK_EOF; i++) {
		if (tokens[i].value) free(tokens[i].value);
	}
	free(tokens);
}
