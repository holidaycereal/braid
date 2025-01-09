#include "word_type.h"
#include <string.h>

TokenType get_word_type(const char* word) {
	if (strcmp(word, "if") == 0)       return WORD_IF;
	if (strcmp(word, "then") == 0)     return WORD_THEN;
	if (strcmp(word, "else") == 0)     return WORD_ELSE;
	if (strcmp(word, "while") == 0)    return WORD_WHILE;
	if (strcmp(word, "for") == 0)      return WORD_FOR;
	if (strcmp(word, "in") == 0)       return WORD_IN;
	if (strcmp(word, "do") == 0)       return WORD_DO;
	if (strcmp(word, "of") == 0)       return WORD_OF;
	if (strcmp(word, "break") == 0)    return WORD_BREAK;
	if (strcmp(word, "continue") == 0) return WORD_CONTINUE;
	if (strcmp(word, "match") == 0)    return WORD_MATCH;
	if (strcmp(word, "test") == 0)     return WORD_TEST;
	if (strcmp(word, "dyn") == 0)      return WORD_DYN;
	if (strcmp(word, "proc") == 0)     return WORD_PROC;
	if (strcmp(word, "data") == 0)     return WORD_DATA;
	if (strcmp(word, "type") == 0)     return WORD_TYPE;
	if (strcmp(word, "from") == 0)     return WORD_FROM;
	if (strcmp(word, "trait") == 0)    return WORD_TRAIT;
	if (strcmp(word, "base") == 0)     return WORD_BASE;
	if (strcmp(word, "impl") == 0)     return WORD_IMPL;
	if (strcmp(word, "self") == 0)     return WORD_SELF;
	if (strcmp(word, "include") == 0)  return WORD_INCLUDE;
	if (strcmp(word, "import") == 0)   return WORD_IMPORT;
	if (strcmp(word, "alias") == 0)    return WORD_ALIAS;
	if (strcmp(word, "exit") == 0)     return WORD_EXIT;

	if (strcmp(word, "u8") == 0)       return PRIM_U8;
	if (strcmp(word, "u16") == 0)      return PRIM_U16;
	if (strcmp(word, "u32") == 0)      return PRIM_U32;
	if (strcmp(word, "u64") == 0)      return PRIM_U64;
	if (strcmp(word, "i8") == 0)       return PRIM_I8;
	if (strcmp(word, "i16") == 0)      return PRIM_I16;
	if (strcmp(word, "i32") == 0)      return PRIM_I32;
	if (strcmp(word, "i64") == 0)      return PRIM_I64;
	if (strcmp(word, "usize") == 0)    return PRIM_USIZE;
	if (strcmp(word, "isize") == 0)    return PRIM_ISIZE;
	if (strcmp(word, "f32") == 0)      return PRIM_F32;
	if (strcmp(word, "f64") == 0)      return PRIM_F64;
	if (strcmp(word, "bool") == 0)     return PRIM_BOOL;

	if (strcmp(word, "true") == 0)     return LIT_TRUE;
	if (strcmp(word, "false") == 0)    return LIT_FALSE;
	if (strcmp(word, "lsl") == 0)      return LSL;
	if (strcmp(word, "lsr") == 0)      return LSR;
	if (strcmp(word, "asl") == 0)      return ASL;
	if (strcmp(word, "asr") == 0)      return ASR;

	return IDENT;
}
