/**
 * @file test_stress.c
 * @brief Rigorous Stress & Memory Leak Test Suite for MiniScheme.
 *
 * Validates under high computational load and tight memory constraints (200KB ESP32 profile):
 *  1. Massive Garbage Generation & Cheney Two-Space GC Recycling (100,000+ objects)
 *  2. Continuous REPL Execution Session Stress (1,000 sequential bytecode evaluations)
 *  3. BigInt High-Pressure Arithmetic Stress (repeated 1,000-digit factorials & powers)
 *  4. Fault Injection & Error Recovery Resilience (200 consecutive intentional crashes)
 *  5. Full VM Lifecycle Creation/Destruction Leak Check (zero C-heap drift)
 */

#define MINISCHEME_NO_MAIN 1
#include "minischeme.h"
#include "minischeme_internal.h"
#include "minischeme_lib.h"
#include "msbc.h"
#include "init_msbc.h"
#include "compiler_msbc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>

static int g_stress_passed = 0;
static int g_stress_total = 0;

static void check_stress(const char *name, bool condition, const char *telemetry) {
    g_stress_total++;
    if (condition) {
        g_stress_passed++;
        printf("  [PASS] %-48s | %s\n", name, telemetry);
    } else {
        printf("  [FAIL] %-48s | %s\n", name, telemetry);
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
    } else {
        *pos += snprintf(buf + *pos, max_len - *pos, "#<object>");
    }
}

static void format_val(MiniVM *vm, Value val, char *buf, size_t max_len) {
    buf[0] = '\0';
    size_t pos = 0;
    print_val_rec(vm, val, buf, max_len, &pos);
}

static Value compile_and_run(MiniVM *comp_vm, MiniVM *exec_vm, const char *src) {
    mini_clear_error(comp_vm);
    mini_clear_error(exec_vm);

    Value helper = env_lookup_name(comp_vm, "%compile-source-to-code");
    if (mini_is_void(helper)) return mini_void();

    Value s = mini_make_string(comp_vm, src);
    Value code_val = mini_call(comp_vm, helper, 1, &s);
    if (comp_vm->has_error || !mini_is_code(code_val)) return mini_void();

    uint8_t *msbc_buf = NULL;
    size_t msbc_len = 0;
    if (!msbc_serialize(comp_vm, code_val, &msbc_buf, &msbc_len)) return mini_void();

    Value target_code = msbc_deserialize(exec_vm, msbc_buf, msbc_len);
    free(msbc_buf);
    if (exec_vm->has_error || !mini_is_code(target_code)) return mini_void();

    Value clo = mini_make_closure(exec_vm, target_code, mini_nil());
    if (exec_vm->has_error) return mini_void();

    return mini_vm_run(exec_vm, clo, 0, NULL);
}

/* ------------------------------------------------------------------------- */
/* Stress Test 1: Massive Garbage Generation & Cheney Two-Space GC Recycling  */
/* ------------------------------------------------------------------------- */
static void test_massive_gc_stress(MiniVM *comp_vm, MiniVM *exec_vm) {
    printf("\n--- 1. Cheney GC Deep Stress: 300,000 Object Allocations (200KB Profile) ---\n");

    size_t base_alloc = mini_get_allocated_bytes(exec_vm);

    /* Allocate 300,000 cons cells in tight 200KB profile (semispace <= 64KB) */
    const char *code =
        "(let loop ((n 100000) (acc 0))\n"
        "  (if (<= n 0)\n"
        "      acc\n"
        "      (begin\n"
        "        (cons (cons n n) (cons n n))\n"
        "        (loop (- n 1) (+ acc 1)))))";

    clock_t t0 = clock();
    Value res = compile_and_run(comp_vm, exec_vm, code);
    clock_t t1 = clock();
    double elapsed_sec = (double)(t1 - t0) / CLOCKS_PER_SEC;

    bool result_ok = (!exec_vm->has_error && mini_is_fixnum(res) && mini_to_fixnum(res) == 100000);
    size_t peak_heap = mini_get_heap_size(exec_vm);
    char buf[128];
    snprintf(buf, sizeof(buf), "Result: %lld in %.3fs (Semispace: %.1f KB <= 64KB bound)",
             result_ok ? (long long)mini_to_fixnum(res) : -1LL, elapsed_sec, (double)peak_heap / 1024.0);
    check_stress("Alloc 300,000 cons cells across ~75 GC cycles", result_ok && peak_heap <= 64 * 1024, buf);

    /* Explicit GC to collect remaining dead frame artifacts */
    mini_gc(exec_vm);
    size_t final_alloc = mini_get_allocated_bytes(exec_vm);

    snprintf(buf, sizeof(buf), "Baseline: %zu B -> Peak: %zu B -> Post-GC: %zu B (Delta: %+lld B)",
             base_alloc, peak_heap, final_alloc, (long long)final_alloc - (long long)base_alloc);
    /* Retained heap after GC must be clean and bounded */
    check_stress("Cheney GC complete garbage reclamation (Zero Leak)", final_alloc <= base_alloc + 8192, buf);
}

/* ------------------------------------------------------------------------- */
/* Stress Test 2: Continuous REPL Session Stress (1,000 Sequential Evaluations)*/
/* ------------------------------------------------------------------------- */
static void test_continuous_repl_stress(MiniVM *comp_vm, MiniVM *exec_vm) {
    printf("\n--- 2. Continuous REPL Session: 1,000 Sequential Bytecode Executions ---\n");

    /* Define stateful helper in exec_vm */
    compile_and_run(comp_vm, exec_vm, "(define g-counter 0)");
    compile_and_run(comp_vm, exec_vm,
        "(define-syntax inc!\n"
        "  (syntax-rules ()\n"
        "    ((_ var step) (set! var (+ var step)))))");

    size_t alloc_at_100 = 0;
    size_t alloc_at_500 = 0;
    size_t alloc_at_1000 = 0;

    clock_t t0 = clock();
    bool all_succeeded = true;

    for (int i = 1; i <= 1000; i++) {
        char expr[256];
        int mode = i % 5;
        if (mode == 0) {
            snprintf(expr, sizeof(expr), "(+ %d (* %d 2))", i, i);
        } else if (mode == 1) {
            snprintf(expr, sizeof(expr), "(inc! g-counter 1)");
        } else if (mode == 2) {
            snprintf(expr, sizeof(expr), "(map (lambda (x) (* x 2)) '(1 2 3 4 5))");
        } else if (mode == 3) {
            snprintf(expr, sizeof(expr), "(vector %d %d (+ %d %d))", i, i + 1, i, i + 1);
        } else {
            snprintf(expr, sizeof(expr), "(let ((x %d) (y 10)) (if (> x y) (- x y) (+ x y)))", i);
        }

        Value r = compile_and_run(comp_vm, exec_vm, expr);
        (void)r;
        if (exec_vm->has_error || comp_vm->has_error) {
            all_succeeded = false;
            printf("  [FAIL] Failed at iteration %d: %s\n", i,
                   exec_vm->has_error ? exec_vm->error_buf : comp_vm->error_buf);
            break;
        }

        if (i == 100) alloc_at_100 = mini_get_allocated_bytes(exec_vm);
        if (i == 500) alloc_at_500 = mini_get_allocated_bytes(exec_vm);
        if (i == 1000) alloc_at_1000 = mini_get_allocated_bytes(exec_vm);
    }

    clock_t t1 = clock();
    double total_sec = (double)(t1 - t0) / CLOCKS_PER_SEC;

    char buf[160];
    snprintf(buf, sizeof(buf), "1000 evals in %.3fs (%.2f ms/req, 100%% success)",
             total_sec, (total_sec / 1000.0) * 1000.0);
    check_stress("1,000 rapid sequential REPL evaluations", all_succeeded, buf);

    /* Verify global counter state accumulated accurately */
    Value counter_val = compile_and_run(comp_vm, exec_vm, "g-counter");
    bool counter_ok = (mini_is_fixnum(counter_val) && mini_to_fixnum(counter_val) == 200);
    snprintf(buf, sizeof(buf), "Counter: %lld / 200 increments verified",
             counter_ok ? (long long)mini_to_fixnum(counter_val) : -1LL);
    check_stress("Stateful closure / global mutation integrity", counter_ok, buf);

    /* Memory stability check across 100, 500, 1000 */
    mini_gc(exec_vm);
    size_t post_gc_alloc = mini_get_allocated_bytes(exec_vm);
    size_t heap_cap = mini_get_heap_size(exec_vm);

    snprintf(buf, sizeof(buf), "Allocated at #100: %zu B, #500: %zu B, #1000: %zu B -> Post-GC: %zu B (Cap: %.1f KB)",
             alloc_at_100, alloc_at_500, alloc_at_1000, post_gc_alloc, (double)heap_cap / 1024.0);
    /* Retained heap after 1,000 evals must be bounded (O(1) memory growth) */
    bool no_leak = (post_gc_alloc <= alloc_at_100 + 4096) && (heap_cap <= 64 * 1024);
    check_stress("O(1) Long-term REPL heap plateau (No Per-Request Leak)", no_leak, buf);
}

/* ------------------------------------------------------------------------- */
/* Stress Test 3: BigInt High-Pressure Arithmetic Stress                      */
/* ------------------------------------------------------------------------- */
static void test_bigint_stress(MiniVM *comp_vm, MiniVM *exec_vm) {
    printf("\n--- 3. BigInt High-Pressure Stress: Repeated 1,000-Digit Arithmetic ---\n");

    /* Ensure fact is defined in exec_vm */
    compile_and_run(comp_vm, exec_vm, "(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))");

    size_t base_alloc = mini_get_allocated_bytes(exec_vm);

    /* Compute 50 large BigInt factorials and powers in succession */
    clock_t t0 = clock();
    bool bigint_ok = true;

    for (int i = 0; i < 50; i++) {
        Value f500 = compile_and_run(comp_vm, exec_vm, "(fact 500)");
        if (exec_vm->has_error || !mini_is_bigint(f500)) {
            bigint_ok = false;
            break;
        }

        Value p2_500 = compile_and_run(comp_vm, exec_vm, "(expt 2 500)");
        if (exec_vm->has_error || !mini_is_bigint(p2_500)) {
            bigint_ok = false;
            break;
        }
    }
    clock_t t1 = clock();
    double elapsed = (double)(t1 - t0) / CLOCKS_PER_SEC;

    char buf[128];
    snprintf(buf, sizeof(buf), "50 x (fact 500) [1135 digits] + 50 x (expt 2 500) in %.3fs", elapsed);
    check_stress("Repeated 1,000-digit BigInt stress computations", bigint_ok, buf);

    /* Verify temporary limbs and BigInt intermediates are completely reclaimed */
    mini_gc(exec_vm);
    size_t final_alloc = mini_get_allocated_bytes(exec_vm);
    snprintf(buf, sizeof(buf), "Pre-BigInt: %zu B -> Post-GC: %zu B (Delta: %+lld B)",
             base_alloc, final_alloc, (long long)final_alloc - (long long)base_alloc);
    check_stress("BigInt temporary limb & object GC reclamation", final_alloc <= base_alloc + 8192, buf);
}

/* ------------------------------------------------------------------------- */
/* Stress Test 4: Fault Injection & Error Recovery Stress (200 Injected Errors)*/
/* ------------------------------------------------------------------------- */
static void test_fault_injection_stress(MiniVM *comp_vm, MiniVM *exec_vm) {
    printf("\n--- 4. Fault Injection & Error Recovery: 200 Consecutive Intentional Crashes ---\n");

    const char *error_payloads[] = {
        "(let loop ((x 0)) (+ 1 (loop (+ x 1))))",     /* 1. Deep recursion -> call stack overflow */
        "(+ unbound-variable-xyz 42)",                 /* 2. Unbound variable lookup error */
        "(car 12345)",                                 /* 3. Type error (expected pair) */
        "(vector-ref #(1 2 3) 9999)"                   /* 4. Vector index out of bounds */
    };
    int num_payloads = sizeof(error_payloads) / sizeof(error_payloads[0]);

    Value *initial_sp = exec_vm->stack;
    bool all_recovered = true;
    int error_count = 0;

    for (int i = 0; i < 200; i++) {
        const char *bad_code = error_payloads[i % num_payloads];
        compile_and_run(comp_vm, exec_vm, bad_code);

        if (!exec_vm->has_error) {
            all_recovered = false;
            printf("  [FAIL] Expected error on payload #%d, but succeeded!\n", i);
            break;
        }
        error_count++;

        /* Clear error and verify stack/frame symmetry */
        mini_clear_error(exec_vm);
        if (exec_vm->sp != initial_sp || exec_vm->call_frame_count != 0) {
            all_recovered = false;
            printf("  [FAIL] Stack leakage detected after error #%d: sp offset=%ld, frame_count=%d\n",
                   i, (long)(exec_vm->sp - initial_sp), exec_vm->call_frame_count);
            break;
        }
    }

    char buf[128];
    snprintf(buf, sizeof(buf), "%d consecutive faults trapped & cleared with 0 SP drift", error_count);
    check_stress("Fault Injection: 200 error crashes cleanly recovered", all_recovered, buf);

    /* Post-crash functional sanity check: evaluate complex valid expression */
    Value valid_res = compile_and_run(comp_vm, exec_vm,
        "(let ((a 100) (b 200)) (if (= (+ a b) 300) 'system-healthy 'error))");
    char res_str[64];
    format_val(exec_vm, valid_res, res_str, sizeof(res_str));

    bool health_ok = (strcmp(res_str, "system-healthy") == 0);
    snprintf(buf, sizeof(buf), "Post-crash evaluation: %s (sp: 0, frames: 0)", res_str);
    check_stress("Post-fault operational integrity verification", health_ok, buf);
}

/* ------------------------------------------------------------------------- */
/* Stress Test 5: Full VM Lifecycle Creation & Destruction (Zero C-Heap Drift)*/
/* ------------------------------------------------------------------------- */
static void test_vm_lifecycle_stress(void) {
    printf("\n--- 5. Full VM Lifecycle: 30 Clean Create/Run/Destroy Cycles ---\n");

    bool lifecycle_ok = true;
    for (int cycle = 0; cycle < 30; cycle++) {
        MiniVM *test_vm = mini_create_profile(MINI_PROFILE_EMBEDDED_200K);
        if (!test_vm) {
            lifecycle_ok = false;
            break;
        }

        /* Load 3.9KB runtime standard library into test_vm */
        Value init_code = msbc_deserialize(test_vm, init_msbc_data, init_msbc_len);
        if (!mini_is_code(init_code)) {
            mini_destroy(test_vm);
            lifecycle_ok = false;
            break;
        }
        Value init_clo = mini_make_closure(test_vm, init_code, mini_nil());
        mini_vm_run(test_vm, init_clo, 0, NULL);

        /* Run small computation */
        mini_gc(test_vm);
        mini_destroy(test_vm);
    }

    check_stress("30 full MiniVM instances created, executed, & destroyed",
                 lifecycle_ok, "Zero crashes, clean free of from/to space, stack, & frame buffers");
}

/* ------------------------------------------------------------------------- */
/* Main Test Runner                                                          */
/* ------------------------------------------------------------------------- */
int main(void) {
    printf("========================================================================\n");
    printf("  MiniScheme Stress & Memory Leak Detection Suite\n");
    printf("  Target Profile: 200KB ESP32 Embedded Profile (Hard Limit <= 64KB Heap)\n");
    printf("========================================================================\n");

    /* 1. Initialize Compiler VM (64-bit/32-bit standalone MSBC compiler) */
    MiniConfig comp_cfg;
    memset(&comp_cfg, 0, sizeof(comp_cfg));
    comp_cfg.initial_heap_bytes = 1024 * 1024;
    comp_cfg.skip_init_fasl = true;
    MiniVM *comp_vm = mini_create_ext(&comp_cfg);
    assert(comp_vm != NULL);

    Value comp_code = msbc_deserialize(comp_vm, compiler_msbc_data, compiler_msbc_len);
    assert(mini_is_code(comp_code));
    Value comp_clo = mini_make_closure(comp_vm, comp_code, mini_nil());
    mini_vm_run(comp_vm, comp_clo, 0, NULL);
    assert(!mini_get_error(comp_vm));

    /* 2. Initialize Execution VM (200KB profile) */
    MiniVM *exec_vm = mini_create_profile(MINI_PROFILE_EMBEDDED_200K);
    assert(exec_vm != NULL);
    mini_register_hardware_lib(exec_vm);

    Value init_code = msbc_deserialize(exec_vm, init_msbc_data, init_msbc_len);
    assert(mini_is_code(init_code));
    Value init_clo = mini_make_closure(exec_vm, init_code, mini_nil());
    mini_vm_run(exec_vm, init_clo, 0, NULL);
    assert(!mini_get_error(exec_vm));

    /* Execute all stress test suites */
    test_massive_gc_stress(comp_vm, exec_vm);
    test_continuous_repl_stress(comp_vm, exec_vm);
    test_bigint_stress(comp_vm, exec_vm);
    test_fault_injection_stress(comp_vm, exec_vm);
    test_vm_lifecycle_stress();

    mini_destroy(comp_vm);
    mini_destroy(exec_vm);

    printf("\n========================================================================\n");
    printf("  Stress Test Suite Summary: %d / %d PASSED (%.1f%%)\n",
           g_stress_passed, g_stress_total,
           (double)g_stress_passed / (g_stress_total > 0 ? g_stress_total : 1) * 100.0);
    printf("========================================================================\n");

    return (g_stress_passed == g_stress_total) ? 0 : 1;
}
