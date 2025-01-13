#ifndef DEBUG_H
#define DEBUG_H

#include <stdbool.h>

typedef enum {
	DEBUG_LEXER  = 1 << 0,  // 0001
	DEBUG_PARSER = 1 << 1,  // 0010
} DebugCategory;

void debug_init(void);

bool is_enabled(DebugCategory category);

void debug_print(DebugCategory category, const char* fmt, ...);

#endif
