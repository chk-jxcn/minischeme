CC ?= gcc
# WERROR= (empty) to build without -Werror; M32=-m32 for the 32-bit device profile
WERROR ?= -Werror
M32 ?=
CFLAGS ?= -std=c99 -Wall -Wextra -pedantic -Wno-unused-parameter -O2
CPPFLAGS += -Iinclude -Ibuild -D__USE_MINGW_ANSI_STDIO=1 -DMINISCHEME_EMBED_INIT
ALL_CFLAGS = $(CFLAGS) $(WERROR) $(M32)
LIBS ?= -lm

BUILD = build
SRCS = src/minischeme.c src/minischeme_vm.c src/minischeme_gc.c src/minischeme_prim.c \
       src/minischeme_bigint.c src/minischeme_math.c src/minischeme_fasl.c \
       src/minischeme_cfasl.c src/minischeme_lib.c src/minischeme_help.c src/minischeme_stubs.c

# The library has no main(): the VM's command line is a separate object
LIB_OBJS = $(SRCS:src/%.c=$(BUILD)/%.o)
MAIN_OBJ = $(BUILD)/minischeme_main.o
CLI_OBJS = $(filter-out $(BUILD)/minischeme_vm.o,$(LIB_OBJS)) $(MAIN_OBJ)

TESTS = $(BUILD)/test_compiler $(BUILD)/test_msbc $(BUILD)/test_stress
GEN = $(BUILD)/init_msbc.h $(BUILD)/compiler_msbc.h

.PHONY: all clean test check

all: minischeme libminischeme.a

$(BUILD):
	mkdir -p $(BUILD)

$(BUILD)/%.o: src/%.c | $(BUILD)
	$(CC) $(ALL_CFLAGS) $(CPPFLAGS) -DMINISCHEME_NO_MAIN -c $< -o $@

$(MAIN_OBJ): src/minischeme_vm.c | $(BUILD)
	$(CC) $(ALL_CFLAGS) $(CPPFLAGS) -c $< -o $@

libminischeme.a: $(LIB_OBJS)
	ar rcs $@ $(LIB_OBJS)

minischeme: $(CLI_OBJS)
	$(CC) $(ALL_CFLAGS) -o $@ $(CLI_OBJS) $(LIBS)

# MSBC images of scm/ for the test suites (tools/gen_msbc.c)
$(BUILD)/gen_msbc: tools/gen_msbc.c $(LIB_OBJS)
	$(CC) $(ALL_CFLAGS) $(CPPFLAGS) -o $@ tools/gen_msbc.c $(LIB_OBJS) $(LIBS)

$(BUILD)/init_msbc.h: $(BUILD)/gen_msbc scm/init.scm
	$(BUILD)/gen_msbc init scm $@

$(BUILD)/compiler_msbc.h: $(BUILD)/gen_msbc scm/compiler.scm scm/init.scm
	$(BUILD)/gen_msbc compiler scm $@

$(BUILD)/test_%: tests/test_%.c $(LIB_OBJS) $(GEN)
	$(CC) $(ALL_CFLAGS) $(CPPFLAGS) -DMINISCHEME_NO_MAIN -o $@ $< $(LIB_OBJS) $(LIBS)

# The command line, then every suite: each exits non-zero on a failed check
test: minischeme $(TESTS)
	@out=$$(./minischeme -e "(display (+ 10 (* 3 5)))"); test "$$out" = "25" || { echo "expected 25, got: $$out"; exit 1; }
	@out=$$(./minischeme -e "(display (letrec ((fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))) (fact 25)))"); \
	  test "$$out" = "15511210043330985984000000" || { echo "expected 25!, got: $$out"; exit 1; }
	$(BUILD)/test_msbc
	$(BUILD)/test_compiler
	$(BUILD)/test_stress

check: test

clean:
	rm -rf $(BUILD) minischeme minischeme.exe libminischeme.a src/*.o
