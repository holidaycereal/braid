#include "lexer.h"
#include "word_type.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

Token* make_token(TokenType type, const char* value) {
	Token* token = malloc(sizeof(Token));
	token->type = type;
	token->value = value ? strdup(value) : NULL;
	return token;
}

Token* lex(const char* input) {
	Token* tokens = malloc(sizeof(Token) * 128);
	size_t capacity = 128;
	size_t count = 0;  // Token count
	size_t pos = 0;    // Character position in input file

	while (input[pos] != '\0') {
		// Skip whitespace
		if (isspace(input[pos])) {
			pos++;
			continue;
		}
		// Skip one-line comments
		if (input[pos] == '/' && input[pos + 1] == '/') {
			pos += 2;
			while (input[pos] != '\0' && input[pos] != '\n') pos++;
			continue;
		}
		// Skip block comments
		if (input[pos] == '/' && input[pos + 1] == '*') {
			pos += 2;
			size_t nest_levels = 1;
			while (input[pos] != '\0') {
				if (input[pos] == '/' && input[pos + 1] == '*') {
					pos += 2;
					nest_levels++;
				} else if (input[pos] == '*' && input[pos + 1] == '/') {
					pos += 2;
					nest_levels--;
					if (nest_levels == 0) break;
				} else {
					pos++;
				}
			}
			continue;
		}

		// Identifiers & builtin words
		if (isalpha(input[pos]) || input[pos] == '_') {
			size_t start = pos;
			while (isalnum(input[pos]) || input[pos] == '_') pos++;

			size_t len = pos - start;
			char* word = malloc(len + 1);
			strncpy(word, input + start, len);
			word[len] = '\0';

			TokenType type = get_word_type(word);
			tokens[count++] = *make_token(type, type == IDENT ? word : NULL);
			free(word);
		}

		// Numeric literals
		// TODO: scientific notation
		else if (isdigit(input[pos])) {
			size_t start = pos;
			bool has_dot = false;  // To ensure e.g. "4.2.8" is not one number

			if (input[pos] == '0'         // Hexadecimal
					&& (input[pos + 1] == 'x' || input[pos + 1] == 'X')) {
				pos += 2;
				while (isxdigit(input[pos])) pos++;
			} else if (input[pos] == '0'  // Octal
					&& (input[pos + 1] == 'o' || input[pos + 1] == 'O')) {
				pos += 2;
				while (input[pos] >= '0' && input[pos] <= '7') pos++;
			} else if (input[pos] == '0'  // Binary
					&& (input[pos + 1] == 'b' || input[pos + 1] == 'B')) {
				pos += 2;
				while (input[pos] == '0' || input[pos] == '1') pos++;
			} else {                      // Decimal
				while (isdigit(input[pos]) || input[pos] == '.' && !has_dot) {
					if (input[pos] == '.') has_dot = true;
					pos++;
				}
			}

			size_t len = pos - start;
			char* num = malloc(len + 1);
			strncpy(num, input + start, len);
			num[len] = '\0';

			tokens[count++] = *make_token(LIT_NUM, num);
			free(num);
		}

		// String literals
		else if (input[pos] == '"' || input[pos] == '`') {
			pos++;  // Skip opening quote
			size_t start = pos;
			TokenType type = input[pos - 1] == '"' ? LIT_STR : LIT_STR_RAW;

			while (input[pos] != '\0'
					&& input[pos] != (type == LIT_STR ? '"' : '`')) pos++;

			if (input[pos] == '\0') {
				tokens[count++] = *make_token(
						ERR_TOKEN, "Unterminated string literal");
				continue;
			}

			size_t len = pos - start;
			char* str = malloc(len + 1);
			strncpy(str, input + start, len);
			str[len] = '\0';

			tokens[count++] = *make_token(type, str);
			free(str);
			pos++;  // Skip closing quote
		}

		// Character literals
		else if (input[pos] == '\'') {
			pos++;  // Skip opening quote
			char ch[3];

			if (input[pos] != '\0' && input[pos + 1] != '\0') {
				if (input[pos] == '\\' && input[pos + 2] == '\'') {
					ch[0] = input[pos];
					ch[1] = input[pos + 1];
					ch[2] = '\0';
					pos += 2;
				} else if (input[pos] != '\'' && input[pos + 1] == '\'') {
					ch[0] = input[pos];
					ch[1] = '\0';
					ch[2] = '\0';
					pos++;
				} else {
					while (input[pos] != '\0' && input[pos] != '\'') pos++;
					if (input[pos] == '\'') pos++;
					tokens[count++] = *make_token(
							ERR_TOKEN, "Invalid character literal");
					continue;
				}
			} else {
				tokens[count++] = *make_token(
						ERR_TOKEN, "Unterminated character literal");
				if (input[pos] != '\0') pos++;
				continue;
			}

			tokens[count++] = *make_token(LIT_CHAR, ch);
			pos++;  // Skip closing quote
		}

		// Punctuation & operators
		else {
			TokenType type;
			switch (input[pos]) {
			case '(': type = PAREN_L; pos++; break;
			case ')': type = PAREN_R; pos++; break;
			case '[': type = BRACKET_L; pos++; break;
			case ']': type = BRACKET_R; pos++; break;
			case '{': type = BRACE_L; pos++; break;
			case '}': type = BRACE_R; pos++; break;
			case '.':
				if (input[pos + 1] == '.') { type = RANGE; pos += 2; }
				else { type = DOT; pos++; }
				break;
			case ',': type = COMMA; pos++; break;
			case ';': type = SEMICOLON; pos++; break;
			case ':':
				if (input[pos + 1] == ':') { type = MODULE; pos += 2; }
				else { type = COLON; pos++; }
				break;
			case '=':
				if (input[pos + 1] == '=') { type = TEST_EQ; pos += 2; }
				else { type = EQUALS; pos++; }
				break;
			case '|':
				if (input[pos + 1] == '|') { type = OR; pos += 2; }
				else { type = VERT_LINE; pos++; }
				break;
			case '$': type = DOLLAR; pos++; break;
			case '&':
				if (input[pos + 1] == '&') { type = AND; pos += 2; }
				else { type = AMPERSAND; pos++; }
				break;
			case '!':
				if (input[pos + 1] == '=') { type = NOT_EQ; pos += 2; }
				else { type = BANG; pos++; }
				break;
			case '?': type = QUESTION; pos++; break;
			case '#':
				if (input[pos + 1] == '#') { type = XOR; pos += 2; }
				else { type = HASH; pos++; }
				break;
			case '^':
				if (input[pos + 1] == '=') { type = CARET_EQUALS; pos += 2; }
				else { type = CARET; pos++; }
				break;
			case '-':
				if (input[pos + 1] == '=') { type = MINUS_EQUALS; pos += 2; }
				else if (input[pos + 1] == '-') { type = DECREMENT; pos += 2; }
				else if (input[pos + 1] == '>') { type = ARROW; pos += 2; }
				else { type = MINUS; pos++; }
				break;
			case '+':
				if (input[pos + 1] == '=') { type = PLUS_EQUALS; pos += 2; }
				else if (input[pos + 1] == '+') { type = INCREMENT; pos += 2; }
				else { type = PLUS; pos++; }
				break;
			case '*':
				if (input[pos + 1] == '=') { type = STAR_EQUALS; pos += 2; }
				else { type = STAR; pos++; }
				break;
			case '/':
				if (input[pos + 1] == '=') { type = SLASH_EQUALS; pos += 2; }
				else { type = SLASH; pos++; }
				break;
			case '%': type = PERCENT; pos++; break;
			case '<':
				if (input[pos + 1] == '=') { type = LESS_EQ; pos += 2; }
				else { type = LESS; pos++; }
				break;
			case '>':
				if (input[pos + 1] == '=') { type = GREATER_EQ; pos += 2; }
				else if (input[pos + 1] == '>') { type = MUT_ARROW; pos += 2; }
				else { type = GREATER; pos++; }
				break;
			default:
				type = ERR_TOKEN;
				pos++;
				break;
			}

			if (type == ERR_TOKEN) {
				char err_str[32];
				sprintf(err_str, "Unknown character %c", input[pos - 1]);
				tokens[count++] = *make_token(ERR_TOKEN, err_str);
			} else {
				tokens[count++] = *make_token(type, NULL);
			}
		}

		if (count >= capacity) {
			capacity *= 2;
			tokens = realloc(tokens, sizeof(Token) * capacity);
		}
	}

	tokens[count++] = *make_token(EOF_TOKEN, NULL);

	return tokens;
}
