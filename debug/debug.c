#include "debug.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

static unsigned int enabled_categories = 0;

static struct {
	const char* name;
	DebugCategory category;
} debug_categories[] = {
	{ "lexer",  DEBUG_LEXER },
	{ "parser", DEBUG_PARSER },
};

// Give the correct value to `enabled_categories` bit field
void debug_init(void) {
	const char* debug_env = getenv("BRAID_DEBUG");
	if (!debug_env) return;
	char* debug_str = strdup(debug_env);
	char* category;
	size_t i;
	for (category = strtok(debug_str, ","); category; category = strtok(NULL, ",")) {
		for (i = 0; i < sizeof(debug_categories) / sizeof(debug_categories[0]); i++) {
			if (strcmp(category, debug_categories[i]) == 0) {
				// Enable if the flag bit is set
				enabled_categories |= debug_categories[i].category;
				break;
			}
		}
	}
	free(debug_str);
}

// Check if the bit flag is set
bool is_enabled(DebugCategory category) {
	return enabled_categories & category != 0;
}

void debug_print(DebugCategory category, const char* fmt, ...) {
	if (!is_enabled(category)) return;
	va_list args;
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
}
