CC ?= gcc
CFLAGS ?= -std=c99 -Wall -Wextra -pedantic -Wno-unused-parameter -O2 -Iinclude

SRCS = src/minischeme.c src/minischeme_vm.c src/minischeme_gc.c src/minischeme_prim.c \
       src/minischeme_bigint.c src/minischeme_math.c src/minischeme_fasl.c \
       src/minischeme_cfasl.c src/minischeme_lib.c src/minischeme_help.c

OBJS = $(SRCS:.c=.o)

.PHONY: all clean test

all: minischeme libminischeme.a

libminischeme.a: $(OBJS)
	ar rcs $@ $(OBJS)

minischeme: $(OBJS)
	$(CC) $(CFLAGS) -DMINISCHEME_MAIN -o $@ $(SRCS)

src/%.o: src/%.c
	$(CC) $(CFLAGS) -c $< -o $@

test: test_compiler test_msbc test_stress
	./test_compiler
	./test_msbc
	./test_stress

test_compiler: tests/test_compiler.c $(OBJS)
	$(CC) $(CFLAGS) -o $@ tests/test_compiler.c $(OBJS)

test_msbc: tests/test_msbc.c $(OBJS)
	$(CC) $(CFLAGS) -o $@ tests/test_msbc.c $(OBJS)

test_stress: tests/test_stress.c $(OBJS)
	$(CC) $(CFLAGS) -o $@ tests/test_stress.c $(OBJS)

clean:
	rm -f $(OBJS) minischeme minischeme.exe libminischeme.a test_compiler* test_msbc* test_stress*
