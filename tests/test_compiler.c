/**
 * @file test_compiler.c
 * @brief Comprehensive Test Suite for MiniScheme Scheme Compiler and VM Execution.
 *
 * Runs 60+ rigorous test cases covering:
 *  - Core arithmetic & compile-time constant folding
 *  - Booleans, logic short-circuiting
 *  - Conditionals (if, when, unless, cond)
 *  - Lexical scoping, let, let*, letrec, named let
 *  - Closures, currying, mutable lexical variables (set!)
 *  - Deep recursion and Tail Call Optimization (TCO)
 *  - R7RS Lists, Vectors, Strings
 *  - Hygienic macros (define-syntax / syntax-rules)
 *  - BigInt arithmetic
 *  - Cheney 2-space GC stress cycles in 200KB profile
 *  - Mock hardware primitives
 */

#define MINISCHEME_NO_MAIN 1
#include "minischeme.h"
#include "minischeme_internal.h"
#include "minischeme_lib.h"
#include "minischeme_gui.h"
#include "msbc.h"
#include "init_msbc.h"
#include "compiler_msbc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

static int g_test_count = 0;
static int g_test_passed = 0;

static void check_result(const char *category, const char *expr, const char *actual, const char *expected) {
    g_test_count++;
    if (strcmp(actual, expected) == 0) {
        g_test_passed++;
        printf("  [PASS] %-25s | %-45s => %s\n", category, expr, actual);
    } else {
        printf("  [FAIL] %-25s | %s\n         Expected: %s\n         Actual:   %s\n",
               category, expr, expected, actual);
    }
}

static void print_val_rec(MiniVM *vm, Value v, char *buf, size_t max_len, size_t *pos) {
    if (*pos >= max_len - 1) return;
    if (mini_is_fixnum(v)) {
        *pos += snprintf(buf + *pos, max_len - *pos, "%lld", (long long)mini_to_fixnum(v));
    } else if (mini_is_bigint(v)) {
        char *str = bigint_to_str(v, 10);
        if (str) {
            *pos += snprintf(buf + *pos, max_len - *pos, "%s", str);
            free(str);
        }
    } else if (v == MINI_CONST_NIL) {
        *pos += snprintf(buf + *pos, max_len - *pos, "()");
    } else if (v == MINI_CONST_TRUE) {
        *pos += snprintf(buf + *pos, max_len - *pos, "#t");
    } else if (v == MINI_CONST_FALSE) {
        *pos += snprintf(buf + *pos, max_len - *pos, "#f");
    } else if (v == MINI_CONST_VOID) {
        *pos += snprintf(buf + *pos, max_len - *pos, "#<void>");
    } else if (mini_is_symbol(v)) {
        *pos += snprintf(buf + *pos, max_len - *pos, "%s", mini_symbol_name(v));
    } else if (mini_is_string(v)) {
        *pos += snprintf(buf + *pos, max_len - *pos, "\"%s\"", mini_string_data(v));
    } else if (mini_is_flonum(v)) {
        *pos += snprintf(buf + *pos, max_len - *pos, "%g", mini_to_flonum(v));
    } else if (mini_is_vector(v)) {
        *pos += snprintf(buf + *pos, max_len - *pos, "#(");
        size_t len = mini_vector_length(v);
        for (size_t i = 0; i < len; i++) {
            if (i > 0) *pos += snprintf(buf + *pos, max_len - *pos, " ");
            print_val_rec(vm, mini_vector_ref(vm, v, i), buf, max_len, pos);
        }
        *pos += snprintf(buf + *pos, max_len - *pos, ")");
    } else if (mini_is_pair(v)) {
        *pos += snprintf(buf + *pos, max_len - *pos, "(");
        Value curr = v;
        bool first = true;
        while (mini_is_pair(curr)) {
            if (!first) *pos += snprintf(buf + *pos, max_len - *pos, " ");
            first = false;
            print_val_rec(vm, mini_car(curr), buf, max_len, pos);
            curr = mini_cdr(curr);
        }
        if (!mini_is_nil(curr)) {
            *pos += snprintf(buf + *pos, max_len - *pos, " . ");
            print_val_rec(vm, curr, buf, max_len, pos);
        }
        *pos += snprintf(buf + *pos, max_len - *pos, ")");
    } else {
        *pos += snprintf(buf + *pos, max_len - *pos, "#<object>");
    }
}

static char *format_value(MiniVM *vm, Value val) {
    static char buf[1024];
    buf[0] = '\0';
    size_t pos = 0;
    print_val_rec(vm, val, buf, sizeof(buf), &pos);
    return buf;
}

static void eval_test(MiniVM *comp_vm, MiniVM *exec_vm, const char *cat, const char *src, const char *expected) {
    mini_clear_error(comp_vm);
    Value s = mini_make_string(comp_vm, src);
    mini_push_root(comp_vm, &s);
    Value helper = env_lookup_name(comp_vm, "%compile-source-to-code");
    if (mini_is_void(helper)) {
        mini_pop_root(comp_vm);
        check_result(cat, src, "Error: %compile-source-to-code not found", expected);
        return;
    }

    Value code_val = mini_call(comp_vm, helper, 1, &s);
    mini_pop_root(comp_vm);
    if (comp_vm->has_error || !mini_is_code(code_val)) {
        char err[600];   /* a prefix and a 512-byte VM error buffer */
        snprintf(err, sizeof(err), "CompileError: %s", comp_vm->has_error ? comp_vm->error_buf : "not code");
        check_result(cat, src, err, expected);
        return;
    }

    /* 2. Serialize to MSBC */
    uint8_t *msbc_buf = NULL;
    size_t msbc_len = 0;
    if (!msbc_serialize(comp_vm, code_val, &msbc_buf, &msbc_len)) {
        check_result(cat, src, "SerializeError", expected);
        return;
    }

    /* 3. Deserialize in Execution VM (200KB profile) */
    Value target_code = msbc_deserialize(exec_vm, msbc_buf, msbc_len);
    free(msbc_buf);

    if (exec_vm->has_error || !mini_is_code(target_code)) {
        char err[600];   /* a prefix and a 512-byte VM error buffer */
        snprintf(err, sizeof(err), "DeserializeError: %s", exec_vm->has_error ? exec_vm->error_buf : "not code");
        check_result(cat, src, err, expected);
        return;
    }

    /* 4. Execute */
    Value clo = mini_make_closure(exec_vm, target_code, mini_nil());
    Value res = mini_vm_run(exec_vm, clo, 0, NULL);
    if (exec_vm->has_error) {
        char err[600];   /* a prefix and a 512-byte VM error buffer */
        snprintf(err, sizeof(err), "RuntimeError: %s", exec_vm->error_buf);
        check_result(cat, src, err, expected);
        return;
    }

    /* 5. Compare result */
    char *res_str = format_value(exec_vm, res);
    check_result(cat, src, res_str, expected);
}

int main(void) {
    setvbuf(stdout, NULL, _IONBF, 0);
    printf("======================================================================\n");
    printf("  MiniScheme Scheme Compiler & 200KB Execution VM Test Suite          \n");
    printf("======================================================================\n");

    /* 1. Initialize Compiler VM */
    MiniConfig comp_cfg;
    memset(&comp_cfg, 0, sizeof(comp_cfg));
    comp_cfg.initial_heap_bytes = 4 * 1024 * 1024;
    comp_cfg.skip_init_fasl = true;
    MiniVM *comp_vm = mini_create_ext(&comp_cfg);
    assert(comp_vm != NULL);

    /* Load portable compiler MSBC */
    Value comp_code = msbc_deserialize(comp_vm, compiler_msbc_data, compiler_msbc_len);
    assert(mini_is_code(comp_code));
    Value comp_clo = mini_make_closure(comp_vm, comp_code, mini_nil());
    mini_vm_run(comp_vm, comp_clo, 0, NULL);
    assert(!mini_get_error(comp_vm));

    /* 2. Initialize Execution VM (200KB Lean profile for ESP32) */
    MiniVM *exec_vm = mini_create_profile(MINI_PROFILE_EMBEDDED_200K);
    assert(exec_vm != NULL);
    mini_register_hardware_lib(exec_vm);
    mini_register_gui_lib(exec_vm);

    /* Load lean 3.9KB runtime standard library into exec_vm */
    Value init_code = msbc_deserialize(exec_vm, init_msbc_data, init_msbc_len);
    assert(mini_is_code(init_code));
    Value init_clo = mini_make_closure(exec_vm, init_code, mini_nil());
    mini_vm_run(exec_vm, init_clo, 0, NULL);
    assert(!mini_get_error(exec_vm));

    printf("\n--- 1. Arithmetic & Constant Folding ---\n");
    eval_test(comp_vm, exec_vm, "Arithmetic", "(+ 10 20)", "30");
    eval_test(comp_vm, exec_vm, "Arithmetic", "(- 100 45)", "55");
    eval_test(comp_vm, exec_vm, "Arithmetic", "(* 7 8)", "56");
    eval_test(comp_vm, exec_vm, "Arithmetic", "(/ 144 12)", "12");
    eval_test(comp_vm, exec_vm, "Arithmetic", "(quotient 17 5)", "3");
    eval_test(comp_vm, exec_vm, "Arithmetic", "(remainder 17 5)", "2");
    eval_test(comp_vm, exec_vm, "Arithmetic", "(modulo -17 5)", "3");
    eval_test(comp_vm, exec_vm, "Arithmetic", "(abs -42)", "42");
    eval_test(comp_vm, exec_vm, "Arithmetic", "(max 10 50 30 20)", "50");
    eval_test(comp_vm, exec_vm, "Arithmetic", "(min 10 50 30 5)", "5");
    eval_test(comp_vm, exec_vm, "ConstFolding", "(+ 10 (* 3 5))", "25");
    eval_test(comp_vm, exec_vm, "ConstFolding", "(- (* 10 10) (+ 20 30))", "50");

    printf("\n--- 2. Booleans & Logic Short-Circuit ---\n");
    eval_test(comp_vm, exec_vm, "Boolean", "(not #f)", "#t");
    eval_test(comp_vm, exec_vm, "Boolean", "(not #t)", "#f");
    eval_test(comp_vm, exec_vm, "Boolean", "(= 10 10)", "#t");
    eval_test(comp_vm, exec_vm, "Boolean", "(< 5 10)", "#t");
    eval_test(comp_vm, exec_vm, "Boolean", "(> 5 10)", "#f");
    eval_test(comp_vm, exec_vm, "Boolean", "(<= 5 5)", "#t");
    eval_test(comp_vm, exec_vm, "Boolean", "(>= 10 9)", "#t");
    eval_test(comp_vm, exec_vm, "Logic", "(and (= 1 1) (< 2 3) (> 5 4))", "#t");
    eval_test(comp_vm, exec_vm, "Logic", "(and #t #f #t)", "#f");
    eval_test(comp_vm, exec_vm, "Logic", "(or #f #f 42 #f)", "42");
    eval_test(comp_vm, exec_vm, "Logic", "(or #f #f #f)", "#f");

    printf("\n--- 3. Conditionals & Control Flow ---\n");
    eval_test(comp_vm, exec_vm, "Conditionals", "(if (= 1 1) 'yes 'no)", "yes");
    eval_test(comp_vm, exec_vm, "Conditionals", "(if (= 1 2) 'yes 'no)", "no");
    eval_test(comp_vm, exec_vm, "Conditionals", "(when #t 123)", "123");
    eval_test(comp_vm, exec_vm, "Conditionals", "(when #f 123)", "#<void>");
    eval_test(comp_vm, exec_vm, "Conditionals", "(unless #f 456)", "456");
    eval_test(comp_vm, exec_vm, "Conditionals", "(cond ((= 1 2) 'a) ((= 2 3) 'b) ((= 3 3) 'c) (else 'd))", "c");
    eval_test(comp_vm, exec_vm, "Conditionals", "(cond ((= 1 2) 'a) (else 'default))", "default");

    printf("\n--- 4. Local Bindings (let, let*, letrec, named let) ---\n");
    eval_test(comp_vm, exec_vm, "Bindings", "(let ((a 10) (b 20)) (+ a b))", "30");
    eval_test(comp_vm, exec_vm, "Bindings", "(let* ((a 5) (b (* a 2)) (c (+ a b))) c)", "15");
    eval_test(comp_vm, exec_vm, "Bindings", "(let ((x 1)) (let ((x 2) (y x)) (+ x y)))", "3");
    eval_test(comp_vm, exec_vm, "NamedLet", "(let loop ((i 0) (sum 0)) (if (> i 10) sum (loop (+ i 1) (+ sum i))))", "55");

    printf("\n--- 5. Closures & Scoping ---\n");
    eval_test(comp_vm, exec_vm, "Closure", "((lambda (x y) (+ (* x x) (* y y))) 3 4)", "25");
    eval_test(comp_vm, exec_vm, "Closure", "(define (make-adder n) (lambda (x) (+ x n)))", "make-adder");
    eval_test(comp_vm, exec_vm, "Closure", "((make-adder 10) 25)", "35");
    eval_test(comp_vm, exec_vm, "Curry", "((((lambda (a) (lambda (b) (lambda (c) (+ a (+ b c))))) 10) 20) 30)", "60");
    eval_test(comp_vm, exec_vm, "Mutation", "(define (make-counter) (let ((c 0)) (lambda () (set! c (+ c 1)) c)))", "make-counter");
    eval_test(comp_vm, exec_vm, "Mutation", "(define cnt (make-counter))", "cnt");
    eval_test(comp_vm, exec_vm, "Mutation", "(cnt)", "1");
    eval_test(comp_vm, exec_vm, "Mutation", "(cnt)", "2");
    eval_test(comp_vm, exec_vm, "Mutation", "(cnt)", "3");

    printf("\n--- 6. Recursion & Tail Call Optimization (TCO) ---\n");
    eval_test(comp_vm, exec_vm, "Recursion", "(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))", "fact");
    eval_test(comp_vm, exec_vm, "Recursion", "(fact 6)", "720");
    eval_test(comp_vm, exec_vm, "Recursion", "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))", "fib");
    eval_test(comp_vm, exec_vm, "Recursion", "(fib 10)", "55");
    eval_test(comp_vm, exec_vm, "TCO", "(define (tco-test n) (if (<= n 0) 'done (tco-test (- n 1))))", "tco-test");
    eval_test(comp_vm, exec_vm, "TCO", "(tco-test 20000)", "done");

    printf("\n--- 7. Lists & Higher-Order Utilities ---\n");
    eval_test(comp_vm, exec_vm, "Lists", "(cons 1 2)", "(1 . 2)");
    eval_test(comp_vm, exec_vm, "Lists", "(list 1 2 3 4)", "(1 2 3 4)");
    eval_test(comp_vm, exec_vm, "Lists", "(car '(a b c))", "a");
    eval_test(comp_vm, exec_vm, "Lists", "(cdr '(a b c))", "(b c)");
    eval_test(comp_vm, exec_vm, "Lists", "(length '(10 20 30 40 50))", "5");
    eval_test(comp_vm, exec_vm, "Lists", "(reverse '(1 2 3 4))", "(4 3 2 1)");
    eval_test(comp_vm, exec_vm, "Lists", "(append '(1 2) '(3 4 5))", "(1 2 3 4 5)");
    eval_test(comp_vm, exec_vm, "Lists", "(map (lambda (x) (* x x)) '(1 2 3 4 5))", "(1 4 9 16 25)");
    eval_test(comp_vm, exec_vm, "Lists", "(filter (lambda (x) (> x 10)) '(5 12 8 20 3 15))", "(12 20 15)");
    eval_test(comp_vm, exec_vm, "Lists", "(fold-left + 0 '(1 2 3 4 5 6 7 8 9 10))", "55");

    printf("\n--- 8. Vectors & Strings ---\n");
    eval_test(comp_vm, exec_vm, "Vectors", "(vector-ref (vector 10 20 30) 1)", "20");
    eval_test(comp_vm, exec_vm, "Vectors", "(let ((v (vector 1 2 3))) (vector-set! v 1 99) (vector-ref v 1))", "99");
    eval_test(comp_vm, exec_vm, "Vectors", "(vector-length (vector 1 2 3 4))", "4");
    eval_test(comp_vm, exec_vm, "Strings", "(string-append \"Mini\" \"Scheme\" \" \" \"2026\")", "\"MiniScheme 2026\"");
    eval_test(comp_vm, exec_vm, "Strings", "(string-length \"1234567890\")", "10");
    /* #\ takes the next character as it is, delimiters included (#\" once ran
     * into a string literal); letters behind a letter make a name */
    eval_test(comp_vm, exec_vm, "Chars", "(list #\\\" #\\( #\\) #\\; #\\  #\\#)", "(34 40 41 59 32 35)");
    eval_test(comp_vm, exec_vm, "Chars", "(list #\\space #\\newline #\\tab #\\x41 #\\A #\\Space #\\nul #\\delete #\\x)",
              "(32 10 9 65 65 32 0 127 120)");
    eval_test(comp_vm, exec_vm, "Chars", "(string-length (list->string (list #\\\" #\\a #\\\")))", "3");
    eval_test(comp_vm, exec_vm, "Chars", "(list #\\(#\\))", "(40 41)");
    eval_test(comp_vm, exec_vm, "Chars", "#\\bogus", "CompileError: unknown character name: #\\bogus");

    printf("\n--- 9. Hygienic Macros (define-syntax / syntax-rules) ---\n");
    eval_test(comp_vm, exec_vm, "Macros",
              "(define-syntax my-when (syntax-rules () ((_ test expr ...) (if test (begin expr ...) #f))))",
              "my-when");
    eval_test(comp_vm, exec_vm, "Macros", "(my-when #t 100)", "100");
    eval_test(comp_vm, exec_vm, "Macros", "(my-when #f 100)", "#f");
    eval_test(comp_vm, exec_vm, "Macros",
              "(define-syntax swap! (syntax-rules () ((_ a b) (let ((t a)) (set! a b) (set! b t)))))",
              "swap!");
    eval_test(comp_vm, exec_vm, "Macros", "(let ((x 1) (y 2)) (swap! x y) (cons x y))", "(2 . 1)");
    eval_test(comp_vm, exec_vm, "Macros",
              "(define-syntax my-when-seq (syntax-rules () ((_ test expr ...) (if test (begin expr ...) #f)))) (my-when-seq #t 100)",
              "100");
    eval_test(comp_vm, exec_vm, "Macros",
              "(begin (define-syntax my-when-begin (syntax-rules () ((_ test expr ...) (if test (begin expr ...) #f)))) (my-when-begin #t 200))",
              "200");
    eval_test(comp_vm, exec_vm, "Macros",
              "(define-macro (my-when-macro c . b) (list 'if c (cons 'begin b) #f)) (my-when-macro #t 300)",
              "300");

    printf("\n--- 10. BigInt Arithmetic ---\n");
    eval_test(comp_vm, exec_vm, "BigInt", "(expt 2 50)", "1125899906842624");
    eval_test(comp_vm, exec_vm, "BigInt", "(fact 20)", "2432902008176640000");
    eval_test(comp_vm, exec_vm, "DeepRecursion", "(> (string-length (number->string (fact 500))) 1000)", "#t");

    printf("\n--- 11. Hardware Mock Primitives ---\n");
    eval_test(comp_vm, exec_vm, "Hardware", "(pin-mode 2 1)", "#<void>");
    eval_test(comp_vm, exec_vm, "Hardware", "(digital-write 2 1)", "#<void>");
    eval_test(comp_vm, exec_vm, "Hardware", "(digital-read 2)", "1");
    eval_test(comp_vm, exec_vm, "Hardware", "(digital-write 2 0)", "#<void>");
    eval_test(comp_vm, exec_vm, "Hardware", "(digital-read 2)", "0");
    mini_mock_set_pin_analog(36, 2048);
    eval_test(comp_vm, exec_vm, "Hardware", "(analog-read 36)", "2048");

    printf("\n--- 12. Cheney Two-Space GC Cycles (200KB Profile Stress) ---\n");
    eval_test(comp_vm, exec_vm, "GCStress",
              "(let loop ((n 5000) (acc 0)) (if (<= n 0) acc (begin (cons n n) (loop (- n 1) (+ acc 1)))))",
              "5000");

    size_t alloc_bytes = mini_get_allocated_bytes(exec_vm);
    size_t heap_cap = mini_get_heap_size(exec_vm);
    printf("  [Telemetry] After 5,000 allocations GC cycles:\n");
    printf("              Allocated: %zu bytes (%.1f KB)\n", alloc_bytes, (double)alloc_bytes / 1024.0);
    printf("              Semispace: %zu bytes (%.1f KB) [Hard bound <= 64KB]\n", heap_cap, (double)heap_cap / 1024.0);
    assert(heap_cap <= 64 * 1024);

#ifdef MINISCHEME_DEVICE_TESTS
    /* The GUI, WiFi, peripheral and font primitives live in the device
     * library (minischeme-esp32 and the WebREPL's WASM build), not in this
     * core: build with -DMINISCHEME_DEVICE_TESTS and those sources to run them. */
    printf("\n--- 13. GUI & Virtual LCD Primitives (3.5-inch 480x320) ---\n");
    eval_test(comp_vm, exec_vm, "GUI", "(gui-init 480 320)", "#<void>");
    eval_test(comp_vm, exec_vm, "GUI", "(gui-width)", "480");
    eval_test(comp_vm, exec_vm, "GUI", "(gui-height)", "320");
    eval_test(comp_vm, exec_vm, "GUI", "(gui-rgb 255 0 0)", "63488");
    eval_test(comp_vm, exec_vm, "GUI", "(gui-clear 0)", "#<void>");
    eval_test(comp_vm, exec_vm, "GUI", "(gui-rect 10 20 100 50 65535 #t)", "#<void>");
    eval_test(comp_vm, exec_vm, "GUI", "(gui-round-rect 10 20 100 50 8 65535 #t)", "#<void>");
    eval_test(comp_vm, exec_vm, "GUI", "(gui-circle 50 50 20 63488 #t)", "#<void>");
    eval_test(comp_vm, exec_vm, "GUI", "(gui-line 0 0 100 100 65535)", "#<void>");
    eval_test(comp_vm, exec_vm, "GUI", "(gui-text 10 10 \"ESP32\" 65535 2)", "#<void>");
    eval_test(comp_vm, exec_vm, "GUI", "(gui-flush)", "#<void>");
    eval_test(comp_vm, exec_vm, "GUI", "(gui-touch-poll)", "#f");
    eval_test(comp_vm, exec_vm, "GUI", "(gui-touch-down?)", "#f");
    mini_gui_inject_touch(true, 120, 240);
    eval_test(comp_vm, exec_vm, "GUI", "(gui-touch-poll)", "(120 . 240)");
    eval_test(comp_vm, exec_vm, "GUI", "(gui-touch-down?)", "#t");
    mini_gui_inject_touch(false, 0, 0);
    eval_test(comp_vm, exec_vm, "GUI", "(gui-touch-poll)", "#f");

    printf("\n--- 13.5. Framebuffer Management, Monochrome & 2^N Reduced Color Depth ---\n");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-framebuffer-enabled?)", "#f");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-framebuffer-ram)", "0");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-framebuffer-enable #t)", "#t");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-framebuffer-enabled?)", "#t");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-color-depth 4)", "4");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-framebuffer-ram)", "76800");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-color-depth 1)", "1");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-framebuffer-ram)", "19200");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-mono-colors 65535 0)", "(65535 0)");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-color-depth 2)", "2");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-framebuffer-ram)", "38400");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-color-depth 8)", "8");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-framebuffer-ram)", "153600");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-color-depth 16)", "16");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-framebuffer-ram)", "307200");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-palette-set! 0 12345)", "#<void>");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-palette-get 0)", "12345");
    /* Color schemes are independent of the color bits */
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-palette-preset 'default-16)", "#t");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-color-scheme)", "color");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-color-depth)", "16");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-palette-preset 'green)", "#t");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-color-scheme)", "green");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-color-bits 2)", "2");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-color-scheme 'gameboy)", "gameboy");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-color-bits)", "2");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-framebuffer-enable #f)", "#f");
    eval_test(comp_vm, exec_vm, "GUI-FB", "(gui-framebuffer-ram)", "0");

    printf("\n--- 14. WiFi Network Simulation Primitives ---\n");
    eval_test(comp_vm, exec_vm, "WiFi", "(wifi-connected?)", "#f");
    eval_test(comp_vm, exec_vm, "WiFi", "(wifi-ip)", "\"0.0.0.0\"");
    eval_test(comp_vm, exec_vm, "WiFi", "(wifi-connect \"DeskInfo-AP\" \"pass1234\")", "#t");
    eval_test(comp_vm, exec_vm, "WiFi", "(wifi-connected?)", "#t");
    eval_test(comp_vm, exec_vm, "WiFi", "(wifi-ip)", "\"192.168.1.105\"");
    eval_test(comp_vm, exec_vm, "WiFi", "(wifi-disconnect)", "#<void>");
    eval_test(comp_vm, exec_vm, "WiFi", "(wifi-connected?)", "#f");

    printf("\n--- 15. ESP32 Desktop Card Peripherals (WS2812, Light, Backlight, Button) ---\n");
    eval_test(comp_vm, exec_vm, "Peripherals", "(display-model)", "\"ILI9488-8BIT-PARALLEL-480x320\"");
    eval_test(comp_vm, exec_vm, "Peripherals", "(ws2812-set 255 128 64)", "#<void>");
    eval_test(comp_vm, exec_vm, "Peripherals", "(ws2812-get)", "(255 128 64)");
    eval_test(comp_vm, exec_vm, "Peripherals", "(ws2812-clear)", "#<void>");
    eval_test(comp_vm, exec_vm, "Peripherals", "(ws2812-get)", "(0 0 0)");
    eval_test(comp_vm, exec_vm, "Peripherals", "(light-read)", "2048");
    eval_test(comp_vm, exec_vm, "Peripherals", "(light-lux)", "500");
    eval_test(comp_vm, exec_vm, "Peripherals", "(backlight-set 200)", "#<void>");
    eval_test(comp_vm, exec_vm, "Peripherals", "(backlight-get)", "200");
    eval_test(comp_vm, exec_vm, "Peripherals", "(button-read)", "0");
    eval_test(comp_vm, exec_vm, "Peripherals", "(button-pressed?)", "#f");
    mini_button_set(true);
    eval_test(comp_vm, exec_vm, "Peripherals", "(button-read)", "1");
    eval_test(comp_vm, exec_vm, "Peripherals", "(button-pressed?)", "#t");
    mini_button_set(false);
    eval_test(comp_vm, exec_vm, "Peripherals", "(button-pressed?)", "#f");

    printf("\n--- 16. Dot-Matrix Fonts & Text Alignment Primitives ---\n");
    eval_test(comp_vm, exec_vm, "Fonts", "(gui-set-font 0)", "#<void>");
    eval_test(comp_vm, exec_vm, "Fonts", "(gui-get-font)", "0");
    eval_test(comp_vm, exec_vm, "Fonts", "(gui-set-font 1)", "#<void>");
    eval_test(comp_vm, exec_vm, "Fonts", "(gui-get-font)", "1");
    eval_test(comp_vm, exec_vm, "Fonts", "(> (gui-text-width \"12:34\" 1) 40)", "#t");
    eval_test(comp_vm, exec_vm, "Fonts", "(> (gui-text-width \"周一\" 0) 20)", "#t");
    eval_test(comp_vm, exec_vm, "Fonts", "(gui-text-styled 10 10 \"14:32\" 65535 1)", "#<void>");
    eval_test(comp_vm, exec_vm, "Fonts", "(gui-text-box 10 10 200 40 \"14:32\" 65535 1 1 1)", "#<void>");

#endif /* MINISCHEME_DEVICE_TESTS */

    printf("\n--- 17. Compressed FASL (CFASL) & Demand-Paged Bytecode ---\n");
    eval_test(comp_vm, exec_vm, "CFASL", "(define s \"ABCDEF1234567890ABCDEF1234567890\")", "s");
    eval_test(comp_vm, exec_vm, "CFASL", "(define c (cfasl-compress s))", "c");
    eval_test(comp_vm, exec_vm, "CFASL", "(define d (cfasl-decompress c (string-length s)))", "d");
    eval_test(comp_vm, exec_vm, "CFASL", "(equal? s d)", "#t");
    eval_test(comp_vm, exec_vm, "CFASL", "(< (string-length c) (string-length s))", "#t");
    eval_test(comp_vm, exec_vm, "CFASL", "(cfasl-stub? (cfasl-make-stub c (string-length s) 0 64 #f #f #()))", "#t");
    eval_test(comp_vm, exec_vm, "CFASL", "(cfasl-stub? 42)", "#f");

    printf("\n--- Let Bound to Frame Slots ---\n");
    eval_test(comp_vm, exec_vm, "LetSlots", "((lambda () (let ((a 1) (b 2)) (+ a b))))", "3");
    eval_test(comp_vm, exec_vm, "LetSlots", "((lambda (x) (let ((x (+ x 1)) (y x)) (list x y))) 5)", "(6 5)");
    eval_test(comp_vm, exec_vm, "LetSlots", "((lambda (x) (+ (let ((x 10)) x) x)) 1)", "11");
    eval_test(comp_vm, exec_vm, "LetSlots", "((lambda () (let* ((a 2) (b (* a 3))) (+ a b))))", "8");
    eval_test(comp_vm, exec_vm, "LetSlots", "((lambda () (let ((n 5)) (let ((f (lambda () n))) (f)))))", "5");
    eval_test(comp_vm, exec_vm, "LetSlots", "((lambda () (let ((n 1)) (let ((get (lambda () n))) (set! n 7) (get)))))", "7");
    eval_test(comp_vm, exec_vm, "LetSlots", "((lambda () (let ((n 1)) ((lambda () (set! n 9))) n)))", "9");
    eval_test(comp_vm, exec_vm, "LetSlots", "(let loop ((i 0) (acc '())) (if (< i 3) (loop (+ i 1) (cons (let ((j i)) (lambda () j)) acc)) (map (lambda (f) (f)) acc)))", "(2 1 0)");
    eval_test(comp_vm, exec_vm, "LetSlots", "((lambda () (map (lambda (f) (f)) (list (let ((a 1)) (lambda () a)) (let ((a 2)) (lambda () a))))))", "(1 2)");
    eval_test(comp_vm, exec_vm, "LetSlots", "(define (let-count n) (let ((m (- n 1))) (if (= m 0) 'done (let-count m))))", "let-count");
    eval_test(comp_vm, exec_vm, "LetSlots", "(let-count 20000)", "done");
    eval_test(comp_vm, exec_vm, "LetSlots", "((lambda () (let ((a 1)) (define b 2) (+ a b))))", "3");
    eval_test(comp_vm, exec_vm, "LetSlots", "((lambda () (let () 42)))", "42");
    eval_test(comp_vm, exec_vm, "LetSlots", "(define (make-ctr) (let ((c 0)) (lambda () (set! c (+ c 1)) c)))", "make-ctr");
    eval_test(comp_vm, exec_vm, "LetSlots", "(define ctr-a (make-ctr))", "ctr-a");
    eval_test(comp_vm, exec_vm, "LetSlots", "(begin (ctr-a) (ctr-a))", "2");
    eval_test(comp_vm, exec_vm, "LetSlots", "(let ((ctr-b (make-ctr))) (ctr-b))", "1");
    eval_test(comp_vm, exec_vm, "LetSlots", "((lambda (k) (let ((v (* k 2))) (let ((w (+ v 1))) (list k v w)))) 4)", "(4 8 9)");

    printf("\n--- include-bytes (compile-time file embedding) ---\n");
    {
        FILE *fp = fopen("test_include_bytes.bin", "wb");
        if (fp) { fwrite("a\0b\0c", 1, 5, fp); fclose(fp); }
        fp = fopen("test_include_long.bin", "wb");
        if (fp) { for (int i = 0; i < 70000; i++) fputc(i & 0xFF, fp); fclose(fp); }
        eval_test(comp_vm, exec_vm, "IncludeBytes", "(string-length (include-bytes \"test_include_bytes.bin\"))", "5");
        eval_test(comp_vm, exec_vm, "IncludeBytes", "(char->integer (string-ref (include-bytes \"test_include_bytes.bin\") 3))", "0");
        eval_test(comp_vm, exec_vm, "IncludeBytes", "(char->integer (string-ref (include-bytes \"test_include_bytes.bin\") 4))", "99");
        /* over 64KB: MSBC long-string constant (run in the large compiler VM) */
        eval_test(comp_vm, comp_vm, "IncludeBytes", "(let ((s (include-bytes \"test_include_long.bin\"))) (list (string-length s) (char->integer (string-ref s 69999))))", "(70000 111)");
        mini_clear_error(exec_vm);
        /* the bytes are in the compiled code: the file is not needed to run it */
        Value src = mini_make_string(comp_vm, "(string-length (include-bytes \"test_include_bytes.bin\"))");
        mini_push_root(comp_vm, &src);
        Value code = mini_call(comp_vm, env_lookup_name(comp_vm, "%compile-source-to-code"), 1, &src);
        mini_pop_root(comp_vm);
        remove("test_include_bytes.bin");
        remove("test_include_long.bin");
        uint8_t *buf = NULL;
        size_t len = 0;
        bool ok = mini_is_code(code) && msbc_serialize(comp_vm, code, &buf, &len);
        Value run = ok ? msbc_deserialize(exec_vm, buf, len) : mini_void();
        free(buf);
        Value res = mini_is_code(run) ? mini_vm_run(exec_vm, mini_make_closure(exec_vm, run, mini_nil()), 0, NULL) : mini_void();
        check_result("IncludeBytes", "file removed after compiling", mini_is_fixnum(res) ? format_value(exec_vm, res) : "error", "5");
        mini_clear_error(comp_vm);
        src = mini_make_string(comp_vm, "(include-bytes \"test_include_missing.bin\")");
        mini_push_root(comp_vm, &src);
        mini_call(comp_vm, env_lookup_name(comp_vm, "%compile-source-to-code"), 1, &src);
        mini_pop_root(comp_vm);
        check_result("IncludeBytes", "missing file", comp_vm->has_error ? "compile error" : "compiled", "compile error");
        mini_clear_error(comp_vm);
    }

    /* Inline arithmetic (API 1.14): (+ a b) ... of the global procedures are instructions; two
     * fixnums are computed in the VM, anything else goes to the primitive with its results
     * and errors. Arguments come through procedure parameters so nothing is constant-folded. */
    printf("\n--- Inline arithmetic ---\n");
    eval_test(comp_vm, exec_vm, "InlineArith", "(define (f a b) (list (+ a b) (- a b) (* a b))) (f 268435455 1)",
              "(268435456 268435454 268435455)");
    eval_test(comp_vm, exec_vm, "InlineArith", "(define (f a b) (+ a b)) (f 1152921504606846975 1)", "1152921504606846976");
    eval_test(comp_vm, exec_vm, "InlineArith", "(define (f a b) (- a b)) (f -1152921504606846976 1)", "-1152921504606846977");
    eval_test(comp_vm, exec_vm, "InlineArith", "(define (f a b) (* a b)) (list (f 65536 65536) (f 4294967296 4294967296))",
              "(4294967296 18446744073709551616)");
    eval_test(comp_vm, exec_vm, "InlineArith", "(define (f a b) (* a b)) (list (f -1152921504606846976 -1) (f -268435456 -1) (f 0 -5))",
              "(1152921504606846976 268435456 0)");
    eval_test(comp_vm, exec_vm, "InlineArith", "(define (f a b) (list (+ a b) (- a b) (* a b))) (f 1 2.5)", "(3.5 -1.5 2.5)");
    eval_test(comp_vm, exec_vm, "InlineArith",
              "(define (c a b) (list (< a b) (> a b) (<= a b) (>= a b) (= a b))) (list (c 1 2) (c 2 2) (c -3 -4))",
              "((#t #f #t #f #f) (#f #f #t #t #t) (#f #t #f #t #f))");
    eval_test(comp_vm, exec_vm, "InlineArith",
              "(define (c a b) (list (< a b) (= a b))) (list (c 1 1.5) (c 2 2.0) (c 1 1152921504606846976000))",
              "((#t #f) (#f #t) (#t #f))");
    eval_test(comp_vm, exec_vm, "InlineArith", "(define (f a b) (+ a b)) (f 1 \"x\")", "RuntimeError: +: expected number");
    mini_clear_error(exec_vm);
    eval_test(comp_vm, exec_vm, "InlineArith", "(define (f a b) (< a b)) (f 'a 1)", "RuntimeError: <: expected numbers");
    mini_clear_error(exec_vm);
    eval_test(comp_vm, exec_vm, "InlineArith", "(define (f + a) (+ a 1)) (f - 10)", "9");
    eval_test(comp_vm, exec_vm, "InlineArith", "(define (mk +) (lambda (a) (+ a 3))) ((mk *) 5)", "15");
    eval_test(comp_vm, exec_vm, "InlineArith", "(define (lp n acc) (if (= n 0) acc (lp (- n 1) (+ acc 2)))) (lp 100000 0)", "200000");
    eval_test(comp_vm, exec_vm, "InlineArith", "(define x 4) (list (+ 1 2 3 x) (- x) (*) (apply + (list 1 2)) (map + '(1 2) '(10 20)))",
              "(10 -4 1 3 (11 22))");
    {
        /* the instruction is emitted, or a call when *inline-arith* is #f (older firmware) */
        Value helper = env_lookup_name(comp_vm, "%compile-source-to-code");
        Value flag = mini_make_symbol(comp_vm, "*inline-arith*");
        mini_push_root(comp_vm, &flag);
        for (int on = 1; on >= 0; on--) {
            mini_clear_error(comp_vm);
            env_define(comp_vm, comp_vm->global_env, flag, on ? mini_true() : mini_false());
            Value src = mini_make_string(comp_vm, "(< x y)");
            Value code = mini_call(comp_vm, helper, 1, &src);
            bool ok = mini_is_code(code);
            const uint8_t *b = ok ? CODE_BYTES((CodeObj *)UNTAG_PTR(code)) : NULL;
            uint32_t n = ok ? ((CodeObj *)UNTAG_PTR(code))->code_len : 0;
            if (on) ok = ok && n >= 2 && b[n - 2] == OP_LT && b[n - 1] == OP_RETURN;
            else ok = ok && n >= 3 && b[n - 3] == OP_TAIL_CALL && b[n - 2] == 2;
            check_result("InlineArith", on ? "(< x y) is OP_LT" : "*inline-arith* #f: a call",
                         ok ? "yes" : "no", "yes");
        }
        env_define(comp_vm, comp_vm->global_env, flag, mini_true());
        mini_pop_root(comp_vm);
    }
    mini_clear_error(comp_vm);

    /* Unbalanced source: an error naming the line, never a hang (a stray ")" once made the
     * reader return empty symbols forever) */
    eval_test(comp_vm, exec_vm, "Reader", "(define x 1))\n(+ x 1)",
              "CompileError: unexpected ')' with no '(' to close at line 1");
    eval_test(comp_vm, exec_vm, "Reader", "(define x 1)\n\n(define (f y)\n  (+ y 1)\n",
              "CompileError: unexpected end of input: no ')' for the '(' at line 3");
    eval_test(comp_vm, exec_vm, "Reader", "(define s 1)\n\"open",
              "CompileError: unexpected end of input: the string literal is not closed, it starts at line 2");
    eval_test(comp_vm, exec_vm, "Reader", "#(1 2", "CompileError: unexpected end of input: no ')' for the '(' at line 1");
    eval_test(comp_vm, exec_vm, "Reader", "(car '(7 8)) ; ) in a comment\n\"a ) in a string\"", "\"a ) in a string\"");
    mini_clear_error(comp_vm);

    mini_destroy(comp_vm);
    mini_destroy(exec_vm);

    printf("\n======================================================================\n");
    printf("  Compiler & Execution Test Suite: %d / %d PASSED (%.1f%%)\n",
           g_test_passed, g_test_count, (double)g_test_passed / g_test_count * 100.0);
    printf("======================================================================\n");

    return (g_test_passed == g_test_count) ? 0 : 1;
}
