CC ?= gcc
CFLAGS ?= -std=c99 -Wall -Wextra -pedantic -Wno-unused-parameter -O2 -Iinclude -D__USE_MINGW_ANSI_STDIO=1

SRCS = src/minischeme.c src/minischeme_vm.c src/minischeme_gc.c src/minischeme_prim.c \
       src/minischeme_bigint.c src/minischeme_math.c src/minischeme_fasl.c \
       src/minischeme_cfasl.c src/minischeme_lib.c src/minischeme_help.c src/minischeme_stubs.c

OBJS = $(SRCS:.c=.o)

.PHONY: all clean test

all: minischeme libminischeme.a

libminischeme.a: $(OBJS)
	ar rcs $@ $(OBJS)

minischeme: $(OBJS)
	$(CC) $(CFLAGS) -DMINISCHEME_MAIN -DMINISCHEME_EMBED_INIT -o $@ $(SRCS)

src/%.o: src/%.c
	$(CC) $(CFLAGS) -c $< -o $@

test: minischeme
	./minischeme -e "(+ 1 2)"
	./minischeme -e "(letrec ((fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))) (fact 10))"

clean:
	rm -f $(OBJS) minischeme minischeme.exe libminischeme.a
