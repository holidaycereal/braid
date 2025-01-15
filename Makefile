CC = gcc
CFLAGS = -Wall -Wextra -I.

ifdef DEBUG
	CFLAGS += -g -DDEBUG
endif

SRCS = lexer/lexer.c \
       lexer/word_type.c \
       lexer/debug/token_string.c \
       debug/debug.c \
       main.c

OBJS = $(SRCS:.c=.o)

TARGET = braid

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(OBJS) -o $(TARGET)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(TARGET)
