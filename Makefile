CC = gcc
CFLAGS = -Wall -Wextra -I.

ifdef DEBUG
	CFLAGS += -g -DDEBUG
endif

SRCS = lexer/lexer.c \
       lexer/word_type.c \
       debug/debug.c \
       debug/token_string.c \
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
