/**
 * @file test_msbc.c
 * @brief Comprehensive Unit Tests for MiniScheme ByteCode (MSBC) Binary Format.
 *
 * Verifies:
 *  1. Header, Magic, Version serialization & corruption rejection.
 *  2. Constant types roundtrip: Fixnum, Flonum, Bool, Nil, Void, Symbol, String, Pair, Vector.
 *  3. Nested CodeObj (lambda / closures) serialization & deserialization.
 *  4. CodeObj metadata flags (num_params, max_stack, is_variadic, has_inner_lambdas).
 *  5. Truncated and malformed buffer safety (no buffer overflows or crashes).
 *  6. Deserialization and execution inside MINI_PROFILE_EMBEDDED_200K VM.
 */

#define MINISCHEME_NO_MAIN 1
#include "minischeme.h"
#include "minischeme_internal.h"
#include "msbc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>

static int g_tests_run = 0;
static int g_tests_passed = 0;

#define TEST_ASSERT(cond, msg) do { \
    g_tests_run++; \
    if (cond) { \
        g_tests_passed++; \
    } else { \
        printf("  [FAIL] Line %d: %s\n", __LINE__, msg); \
    } \
} while (0)

static Value helper_make_code(MiniVM *vm, Value bytes, Value consts, int num_params, int max_stack, bool is_var, bool has_inner) {
    Value args[6] = {
        bytes, consts,
        mini_make_fixnum(num_params),
        mini_make_fixnum(max_stack),
        is_var ? mini_true() : mini_false(),
        has_inner ? mini_true() : mini_false()
    };
    return mini_call_named(vm, "%make-code", 6, args);
}

/* ========================================================================= */
/* Test 1: Header, Magic, and Truncation Safety                             */
/* ========================================================================= */
static void test_msbc_header_and_safety(MiniVM *vm) {
    printf("[MSBC Test] 1. Header, Magic & Truncation Safety...\n");

    /* Create dummy code object */
    uint8_t opcodes[] = { OP_INT8, 42, OP_RETURN };
    Value bytes_vec = mini_make_vector(vm, sizeof(opcodes), mini_void());
    for (size_t i = 0; i < sizeof(opcodes); i++) {
        mini_vector_set(vm, bytes_vec, i, mini_make_fixnum(opcodes[i]));
    }
    Value consts_vec = mini_make_vector(vm, 0, mini_void());
    Value code_val = helper_make_code(vm, bytes_vec, consts_vec, 0, 64, false, false);
    TEST_ASSERT(mini_is_code(code_val), "Dummy code object created");

    uint8_t *buf = NULL;
    size_t len = 0;
    bool ok = msbc_serialize(vm, code_val, &buf, &len);
    TEST_ASSERT(ok && buf != NULL && len > 8, "Serialization succeeded");

    /* Normal deserialization */
    Value d1 = msbc_deserialize(vm, buf, len);
    TEST_ASSERT(mini_is_code(d1), "Deserialization of valid MSBC succeeded");

    /* Corrupted Magic */
    uint8_t corrupted[128];
    memcpy(corrupted, buf, len < sizeof(corrupted) ? len : sizeof(corrupted));
    corrupted[0] = 'X'; /* Break magic */
    Value bad_magic = msbc_deserialize(vm, corrupted, len);
    TEST_ASSERT(!mini_is_code(bad_magic) && mini_get_error(vm) != NULL, "Corrupted magic rejected safely");
    mini_clear_error(vm);

    /* Corrupted Version */
    memcpy(corrupted, buf, len < sizeof(corrupted) ? len : sizeof(corrupted));
    corrupted[4] = 99; /* Unsupported version */
    Value bad_ver = msbc_deserialize(vm, corrupted, len);
    TEST_ASSERT(!mini_is_code(bad_ver) && mini_get_error(vm) != NULL, "Unsupported version rejected safely");
    mini_clear_error(vm);

    /* Truncated Buffers (test all lengths from 0 to len-1) */
    for (size_t tr = 0; tr < len; tr++) {
        Value trunc_val = msbc_deserialize(vm, buf, tr);
        TEST_ASSERT(!mini_is_code(trunc_val) || tr == 0, "Truncated buffer handled safely without crash");
        mini_clear_error(vm);
    }

    free(buf);
}

/* ========================================================================= */
/* Test 2: Scalar and Compound Constants Roundtrip                          */
/* ========================================================================= */
static void test_msbc_constants_roundtrip(MiniVM *vm) {
    printf("[MSBC Test] 2. Constants Roundtrip (Fixnum, Flonum, Bool, Nil, Void, String, Symbol, Pair, Vector)...\n");

    /* Construct constant array with all supported types */
    Value c_fix_pos = mini_make_fixnum(12345678LL);
    Value c_fix_neg = mini_make_fixnum(-98765432LL);
    Value c_fix_zero = mini_make_fixnum(0);
    Value c_flo = mini_make_flonum(vm, 3.141592653589793);
    Value c_true = mini_true();
    Value c_false = mini_false();
    Value c_nil = mini_nil();
    Value c_void = mini_void();
    Value c_sym = mini_make_symbol(vm, "test-identifier-alpha!");
    Value c_str_empty = mini_make_string(vm, "");
    Value c_str_hello = mini_make_string(vm, "Hello MiniScheme Embedded Bytecode");
    Value c_pair = mini_make_cons(vm, mini_make_fixnum(1), mini_make_cons(vm, mini_make_fixnum(2), mini_nil()));
    
    Value c_vec = mini_make_vector(vm, 3, mini_void());
    mini_vector_set(vm, c_vec, 0, mini_make_fixnum(100));
    mini_vector_set(vm, c_vec, 1, mini_make_string(vm, "inner-str"));
    mini_vector_set(vm, c_vec, 2, mini_true());

    Value all_consts[] = {
        c_fix_pos, c_fix_neg, c_fix_zero, c_flo, c_true, c_false,
        c_nil, c_void, c_sym, c_str_empty, c_str_hello, c_pair, c_vec
    };
    size_t num_c = sizeof(all_consts) / sizeof(all_consts[0]);

    Value consts_vec = mini_make_vector(vm, (uint32_t)num_c, mini_void());
    for (size_t i = 0; i < num_c; i++) {
        mini_vector_set(vm, consts_vec, i, all_consts[i]);
    }

    uint8_t opcodes[] = { OP_CONST, 0, 0, OP_RETURN };
    Value bytes_vec = mini_make_vector(vm, sizeof(opcodes), mini_void());
    for (size_t i = 0; i < sizeof(opcodes); i++) {
        mini_vector_set(vm, bytes_vec, i, mini_make_fixnum(opcodes[i]));
    }

    Value code_val = helper_make_code(vm, bytes_vec, consts_vec, 2, 128, true, false);
    TEST_ASSERT(mini_is_code(code_val), "Code with all constant types created");

    uint8_t *buf = NULL;
    size_t len = 0;
    bool ok = msbc_serialize(vm, code_val, &buf, &len);
    TEST_ASSERT(ok && buf != NULL, "Serialization of constant table succeeded");

    /* Deserialize */
    Value deserialized = msbc_deserialize(vm, buf, len);
    TEST_ASSERT(mini_is_code(deserialized), "Deserialized into CodeObj");

    CodeObj *co = (CodeObj *)UNTAG_PTR(deserialized);
    TEST_ASSERT(co->num_consts == num_c, "Constant count matches");
    TEST_ASSERT(co->num_params == 2, "num_params matches");
    TEST_ASSERT(co->max_stack == 128, "max_stack matches");
    TEST_ASSERT(co->is_variadic == true, "is_variadic flag preserved");

    Value *dc = CODE_CONSTS(co);
    /* Verify individual constants */
    TEST_ASSERT(mini_is_fixnum(dc[0]) && mini_to_fixnum(dc[0]) == 12345678LL, "Fixnum positive matches");
    TEST_ASSERT(mini_is_fixnum(dc[1]) && mini_to_fixnum(dc[1]) == -98765432LL, "Fixnum negative matches");
    TEST_ASSERT(mini_is_fixnum(dc[2]) && mini_to_fixnum(dc[2]) == 0, "Fixnum zero matches");
    TEST_ASSERT(mini_is_flonum(dc[3]) && fabs(mini_to_flonum(dc[3]) - 3.141592653589793) < 1e-12, "Flonum matches");
    TEST_ASSERT(mini_is_true(dc[4]), "Bool true matches");
    TEST_ASSERT(mini_is_false(dc[5]), "Bool false matches");
    TEST_ASSERT(mini_is_nil(dc[6]), "Nil matches");
    TEST_ASSERT(mini_is_void(dc[7]), "Void matches");
    TEST_ASSERT(mini_is_symbol(dc[8]) && strcmp(mini_symbol_name(dc[8]), "test-identifier-alpha!") == 0, "Symbol matches");
    TEST_ASSERT(mini_is_string(dc[9]) && strcmp(mini_string_data(dc[9]), "") == 0, "Empty string matches");
    TEST_ASSERT(mini_is_string(dc[10]) && strcmp(mini_string_data(dc[10]), "Hello MiniScheme Embedded Bytecode") == 0, "String matches");

    /* Pair check (1 2) */
    TEST_ASSERT(mini_is_pair(dc[11]), "Pair matches");
    TEST_ASSERT(mini_to_fixnum(mini_car(dc[11])) == 1, "Pair car matches");
    TEST_ASSERT(mini_to_fixnum(mini_car(mini_cdr(dc[11]))) == 2, "Pair cadr matches");

    /* Vector check #(100 "inner-str" #t) */
    TEST_ASSERT(mini_is_vector(dc[12]), "Vector matches");
    TEST_ASSERT(mini_vector_length(dc[12]) == 3, "Vector length matches");
    TEST_ASSERT(mini_to_fixnum(mini_vector_ref(vm, dc[12], 0)) == 100, "Vector element 0 matches");
    TEST_ASSERT(strcmp(mini_string_data(mini_vector_ref(vm, dc[12], 1)), "inner-str") == 0, "Vector element 1 matches");
    TEST_ASSERT(mini_is_true(mini_vector_ref(vm, dc[12], 2)), "Vector element 2 matches");

    free(buf);
}

/* ========================================================================= */
/* Test 3: Nested CodeObj Roundtrip (Inner Lambda / Closures)               */
/* ========================================================================= */
static void test_msbc_nested_code(MiniVM *vm) {
    printf("[MSBC Test] 3. Nested CodeObj Roundtrip (Inner Lambdas)...\n");

    /* Inner code object: (lambda (x) (* x x)) */
    uint8_t inner_ops[] = { OP_GET_LOCAL, 0, OP_GET_LOCAL, 0, OP_CALL, 2, OP_RETURN };
    Value in_bytes = mini_make_vector(vm, sizeof(inner_ops), mini_void());
    for (size_t i = 0; i < sizeof(inner_ops); i++) {
        mini_vector_set(vm, in_bytes, i, mini_make_fixnum(inner_ops[i]));
    }
    Value in_consts = mini_make_vector(vm, 1, mini_make_symbol(vm, "*"));
    Value inner_code = helper_make_code(vm, in_bytes, in_consts, 1, 64, false, false);

    /* Outer code object containing inner_code in its constant pool */
    uint8_t outer_ops[] = { OP_CLOSURE, 0, 0, OP_RETURN };
    Value out_bytes = mini_make_vector(vm, sizeof(outer_ops), mini_void());
    for (size_t i = 0; i < sizeof(outer_ops); i++) {
        mini_vector_set(vm, out_bytes, i, mini_make_fixnum(outer_ops[i]));
    }
    Value out_consts = mini_make_vector(vm, 1, inner_code);
    Value outer_code = helper_make_code(vm, out_bytes, out_consts, 0, 64, false, true);

    uint8_t *buf = NULL;
    size_t len = 0;
    bool ok = msbc_serialize(vm, outer_code, &buf, &len);
    TEST_ASSERT(ok && buf != NULL, "Nested CodeObj serialized");

    Value deser_outer = msbc_deserialize(vm, buf, len);
    TEST_ASSERT(mini_is_code(deser_outer), "Outer code deserialized");

    CodeObj *out_co = (CodeObj *)UNTAG_PTR(deser_outer);
    TEST_ASSERT(out_co->has_inner_lambdas == true, "has_inner_lambdas flag preserved");
    TEST_ASSERT(out_co->num_consts == 1, "Outer has 1 constant");

    Value inner_deser = CODE_CONSTS(out_co)[0];
    TEST_ASSERT(mini_is_code(inner_deser), "Inner constant deserialized as CodeObj");

    CodeObj *in_co = (CodeObj *)UNTAG_PTR(inner_deser);
    TEST_ASSERT(in_co->num_params == 1, "Inner num_params == 1");
    TEST_ASSERT(in_co->code_len == sizeof(inner_ops), "Inner code_len matches");
    TEST_ASSERT(in_co->num_consts == 1, "Inner num_consts == 1");
    TEST_ASSERT(strcmp(mini_symbol_name(CODE_CONSTS(in_co)[0]), "*") == 0, "Inner symbol '*' matches");

    free(buf);
}

/* ========================================================================= */
/* Test 4: Execution Inside 200KB ESP32 Embedded Profile VM                  */
/* ========================================================================= */
static void test_msbc_execution_in_200k_vm(MiniVM *vm) {
    printf("[MSBC Test] 4. Execution Inside 200KB ESP32 Embedded Profile VM...\n");

    /* Create bytecode for: (+ 123 456) -> 579 */
    uint8_t ops[] = {
        OP_GET_GLOBAL, 0, 0,  /* #0: + */
        OP_INT8, 123,
        OP_CONST, 1, 0,       /* #1: 456 */
        OP_CALL, 2,
        OP_RETURN
    };
    Value bytes_vec = mini_make_vector(vm, sizeof(ops), mini_void());
    for (size_t i = 0; i < sizeof(ops); i++) {
        mini_vector_set(vm, bytes_vec, i, mini_make_fixnum(ops[i]));
    }
    Value consts_vec = mini_make_vector(vm, 2, mini_void());
    mini_vector_set(vm, consts_vec, 0, mini_make_symbol(vm, "+"));
    mini_vector_set(vm, consts_vec, 1, mini_make_fixnum(456));

    Value code_val = helper_make_code(vm, bytes_vec, consts_vec, 0, 64, false, false);

    uint8_t *buf = NULL;
    size_t len = 0;
    bool ok = msbc_serialize(vm, code_val, &buf, &len);
    TEST_ASSERT(ok, "Bytecode serialized");

    /* Create target 200KB ESP32 VM */
    MiniVM *esp_vm = mini_create_profile(MINI_PROFILE_EMBEDDED_200K);
    TEST_ASSERT(esp_vm != NULL, "200KB profile VM created");

    /* Deserialize in target VM */
    Value target_code = msbc_deserialize(esp_vm, buf, len);
    TEST_ASSERT(!mini_get_error(esp_vm) && mini_is_code(target_code), "MSBC deserialized into 200KB VM");

    /* Execute closure */
    Value clo = mini_make_closure(esp_vm, target_code, mini_nil());
    Value res = mini_vm_run(esp_vm, clo, 0, NULL);
    TEST_ASSERT(!mini_get_error(esp_vm), "Executed without error in 200KB VM");
    TEST_ASSERT(mini_is_fixnum(res) && mini_to_fixnum(res) == 579, "Result 123 + 456 == 579");

    size_t alloc_bytes = mini_get_allocated_bytes(esp_vm);
    size_t heap_cap = mini_get_heap_size(esp_vm);
    TEST_ASSERT(alloc_bytes < 32768, "Heap allocation well under 32KB");
    TEST_ASSERT(heap_cap <= 64 * 1024, "Heap capacity within 64KB semispace bound");

    free(buf);
    mini_destroy(esp_vm);
}

/* ========================================================================= */
/* Test 5: version 2 (symbol table) against version 1 (names inline)         */
/* ========================================================================= */
static void test_msbc_symbol_table(MiniVM *vm) {
    printf("[MSBC Test] 5. Symbol table (version 2) vs names inline (version 1)...\n");

    /* outer: consts (string-append inner-code display); inner: (string-append display "x") */
    uint8_t ops[] = { OP_RETURN };
    Value bytes_vec = mini_make_vector(vm, 1, mini_make_fixnum(ops[0]));
    mini_push_root(vm, &bytes_vec);
    Value inner_consts = mini_make_vector(vm, 3, mini_void());
    mini_push_root(vm, &inner_consts);
    mini_vector_set(vm, inner_consts, 0, mini_make_symbol(vm, "string-append"));
    mini_vector_set(vm, inner_consts, 1, mini_make_symbol(vm, "display"));
    mini_vector_set(vm, inner_consts, 2, mini_make_string(vm, "x"));
    Value inner = helper_make_code(vm, bytes_vec, inner_consts, 0, 8, false, false);
    mini_push_root(vm, &inner);
    Value outer_consts = mini_make_vector(vm, 3, mini_void());
    mini_push_root(vm, &outer_consts);
    mini_vector_set(vm, outer_consts, 0, mini_make_symbol(vm, "string-append"));
    mini_vector_set(vm, outer_consts, 1, inner);
    mini_vector_set(vm, outer_consts, 2, mini_make_symbol(vm, "display"));
    Value outer = helper_make_code(vm, bytes_vec, outer_consts, 0, 8, false, true);
    mini_pop_root(vm);
    mini_pop_root(vm);
    mini_pop_root(vm);
    mini_pop_root(vm);
    TEST_ASSERT(mini_is_code(outer), "Code with repeated symbols created");

    uint8_t *v1 = NULL, *v2 = NULL;
    size_t n1 = 0, n2 = 0;
    TEST_ASSERT(msbc_serialize_version(vm, outer, &v1, &n1, 1), "Version 1 written");
    TEST_ASSERT(msbc_serialize(vm, outer, &v2, &n2), "Version 2 (the default) written");
    TEST_ASSERT(v1[4] == 1 && v2[4] == 2, "Version fields 1 and 2");
    TEST_ASSERT(n2 < n1, "Version 2 is smaller (each name once)");
    TEST_ASSERT(v2[8] == 2 && v2[9] == 0, "Two symbols in the table");

    for (int ver = 1; ver <= 2; ver++) {
        Value c = msbc_deserialize(vm, ver == 1 ? v1 : v2, ver == 1 ? n1 : n2);
        TEST_ASSERT(mini_is_code(c) && !mini_get_error(vm), ver == 1 ? "Version 1 read" : "Version 2 read");
        if (!mini_is_code(c)) continue;
        CodeObj *co = (CodeObj *)UNTAG_PTR(c);
        Value *k = CODE_CONSTS(co);
        TEST_ASSERT(k[0] == mini_make_symbol(vm, "string-append") && k[2] == mini_make_symbol(vm, "display"),
                    "Outer symbols interned");
        TEST_ASSERT(mini_is_code(k[1]), "Inner code read");
        if (mini_is_code(k[1])) {
            Value *ik = CODE_CONSTS((CodeObj *)UNTAG_PTR(k[1]));
            TEST_ASSERT(ik[0] == k[0] && ik[1] == k[2] && mini_is_string(ik[2]), "Inner constants via the table");
        }
    }

    /* a symbol index past the table is rejected */
    uint8_t *bad = (uint8_t *)malloc(n2);
    memcpy(bad, v2, n2);
    for (size_t i = 8; i + 2 < n2; i++) {
        if (bad[i] == MSBC_CONST_SYMREF && bad[i + 1] == 0 && bad[i + 2] == 0) { bad[i + 1] = 9; break; }
    }
    Value bc = msbc_deserialize(vm, bad, n2);
    TEST_ASSERT(!mini_is_code(bc) && mini_get_error(vm), "Out-of-range symbol index rejected");
    mini_clear_error(vm);
    free(bad);
    free(v1);
    free(v2);
}

int main(void) {
    setvbuf(stdout, NULL, _IONBF, 0);
    printf("====================================================\n");
    printf("  MiniScheme MSBC Binary Protocol Unit Test Suite   \n");
    printf("====================================================\n");

    MiniVM *vm = mini_create(256 * 1024);
    assert(vm != NULL);

    test_msbc_header_and_safety(vm);
    test_msbc_constants_roundtrip(vm);
    test_msbc_nested_code(vm);
    test_msbc_execution_in_200k_vm(vm);
    test_msbc_symbol_table(vm);

    mini_destroy(vm);

    printf("\n----------------------------------------------------\n");
    printf("MSBC Unit Tests Finished: %d / %d PASSED (%.1f%%)\n",
           g_tests_passed, g_tests_run, (double)g_tests_passed / g_tests_run * 100.0);
    printf("----------------------------------------------------\n");

    return (g_tests_passed == g_tests_run) ? 0 : 1;
}
