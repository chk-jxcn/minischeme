/**
 * @file test_gc.c
 * @brief Collector behaviour on a fixed-size heap (the device profile: initial = maximum,
 *        4 KB eden): how often the old generation is collected, that live data survives
 *        churn, fragmentation and collections forced at awkward moments.
 *
 * The same checks run against either old-generation collector (the build decides:
 * sliding mark-compact, or mark-sweep with -DMINI_GC_MARKSWEEP=1).
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

static int g_passed = 0;
static int g_total = 0;
static MiniVM *g_comp = NULL;

static void check(const char *name, bool ok, const char *note) {
    g_total++;
    if (ok) g_passed++;
    printf("  [%s] %-52s | %s\n", ok ? "PASS" : "FAIL", name, note);
}

static Value run(MiniVM *vm, const char *src) {
    mini_clear_error(g_comp);
    mini_clear_error(vm);
    Value helper = env_lookup_name(g_comp, "%compile-source-to-code");
    if (mini_is_void(helper)) return mini_void();
    Value s = mini_make_string(g_comp, src);
    Value code_val = mini_call(g_comp, helper, 1, &s);
    if (g_comp->has_error || !mini_is_code(code_val)) {
        printf("    compile error: %s\n", mini_get_error(g_comp) ? mini_get_error(g_comp) : "?");
        return mini_void();
    }
    uint8_t *buf = NULL;
    size_t len = 0;
    if (!msbc_serialize(g_comp, code_val, &buf, &len)) return mini_void();
    Value code = msbc_deserialize(vm, buf, len);
    free(buf);
    if (vm->has_error || !mini_is_code(code)) return mini_void();
    Value clo = mini_make_closure(vm, code, mini_nil());
    if (vm->has_error) return mini_void();
    Value r = mini_vm_run(vm, clo, 0, NULL);
    if (vm->has_error) printf("    run error: %s\n", mini_get_error(vm));
    return r;
}

static long long run_int(MiniVM *vm, const char *src) {
    Value r = run(vm, src);
    if (vm->has_error || !mini_is_fixnum(r)) return -999999999LL;
    return (long long)mini_to_fixnum(r);
}

static MiniVM *make_vm(size_t heap, size_t eden) {
    MiniConfig cfg;
    memset(&cfg, 0, sizeof(cfg));
    heap = heap * (sizeof(Value) / 4);       /* the same number of objects with 8-byte values */
    cfg.initial_heap_bytes = heap;
    cfg.max_heap_bytes = heap;
    cfg.eden_bytes = eden;
    cfg.stack_size = 1024;
    cfg.frame_capacity = 256;
    cfg.skip_init_fasl = true;
    MiniVM *vm = mini_create_ext(&cfg);
    assert(vm != NULL);
    mini_register_hardware_lib(vm);
    Value init_code = msbc_deserialize(vm, init_msbc_data, init_msbc_len);
    assert(mini_is_code(init_code));
    Value init_clo = mini_make_closure(vm, init_code, mini_nil());
    mini_vm_run(vm, init_clo, 0, NULL);
    assert(!mini_get_error(vm));
    return vm;
}

static MiniStats stats(MiniVM *vm) {
    MiniStats st;
    mini_get_stats(vm, &st);
    return st;
}

/* Scheme helpers shared by the cases */
static const char *PRELUDE =
    "(begin"
    " (define (churn n) (let loop ((i 0)) (if (< i n) (begin (make-vector 100 i) (loop (+ i 1))))))"
    " (define (fill-vectors! big len) (let f ((i 0)) (if (< i (vector-length big))"
    "    (begin (vector-set! big i (make-vector len i)) (f (+ i 1))))))"
    " (define (sum-first big) (let f ((i 0) (s 0)) (if (< i (vector-length big))"
    "    (f (+ i 1) (+ s (let ((v (vector-ref big i))) (if (vector? v) (vector-ref v 0) 0)))) s)))"
    " (define (iota-list n) (let loop ((i 0) (l '())) (if (= i n) l (loop (+ i 1) (cons i l)))))"
    " (define (sum-list l) (let loop ((l l) (s 0)) (if (null? l) s (loop (cdr l) (+ s (car l))))))"
    " 0)";

/* 1. A heap three quarters full of live data: churn must not collect it over and over */
static void test_full_heap_churn(void) {
    printf("\n--- 1. Churn on a heap that is 76%% live ---\n");
    size_t heap = 4u * 1024 * 1024;
    MiniVM *vm = make_vm(heap, 4096);
    run(vm, PRELUDE);
    /* 1500 vectors of 500 slots: about 3.05 MB with 4-byte values */
    /* slot counts as for 4-byte values: make_vm scales the heap with the value size */
    size_t per = (size_t)(heap * 0.76 / 1500);
    char src[256];
    snprintf(src, sizeof(src), "(begin (define big (make-vector 1500 0)) (fill-vectors! big %u) 1)",
             (unsigned)((per - 8) / 4));
    bool ok = run_int(vm, src) == 1;
    MiniStats s0 = stats(vm);
    ok = ok && run_int(vm, "(begin (churn 40000) (sum-first big))") == 1499LL * 1500 / 2;
    MiniStats s1 = stats(vm);
    unsigned majors = s1.major_gcs - s0.major_gcs, minors = s1.minor_gcs - s0.minor_gcs;
    char note[160];
    snprintf(note, sizeof(note), "%u minor, %u major collections for %u KB of garbage", minors, majors,
             (unsigned)((s1.alloc_bytes - s0.alloc_bytes) / 1024));
    check("live data intact after the churn", ok, note);
    /* the garbage is 16 MB and 0.9 MB is free: a handful of old-generation collections at most */
    check("the old generation is not collected at every minor GC", majors <= 40, note);
    mini_destroy(vm);
}

/* 2. More live data than the 80% mark */
static void test_nearly_full_heap(void) {
    printf("\n--- 2. Churn on a heap that is 90%% live ---\n");
    size_t heap = 1024 * 1024;
    MiniVM *vm = make_vm(heap, 4096);
    run(vm, PRELUDE);
    /* slot counts as for 4-byte values: make_vm scales the heap with the value size */
    size_t per = (size_t)(heap * 0.88 / 400);
    char src[256];
    snprintf(src, sizeof(src), "(begin (define big (make-vector 400 0)) (fill-vectors! big %u) 1)",
             (unsigned)((per - 8) / 4));
    bool ok = run_int(vm, src) == 1;
    MiniStats s0 = stats(vm);
    ok = ok && run_int(vm, "(begin (churn 20000) (sum-first big))") == 399LL * 400 / 2;
    MiniStats s1 = stats(vm);
    unsigned majors = s1.major_gcs - s0.major_gcs, minors = s1.minor_gcs - s0.minor_gcs;
    char note[160];
    snprintf(note, sizeof(note), "%u minor, %u major collections for %u KB of garbage", minors, majors,
             (unsigned)((s1.alloc_bytes - s0.alloc_bytes) / 1024));
    check("live data intact after the churn", ok, note);
    check("major collections stay rare", majors * 20 <= minors + 20, note);
    mini_destroy(vm);
}

/* 3. Holes: every other object dropped, then objects of other sizes allocated into them */
static void test_fragmentation(void) {
    printf("\n--- 3. Fragmentation: holes reused, big objects after small holes ---\n");
    MiniVM *vm = make_vm(1024 * 1024, 4096);
    run(vm, PRELUDE);
    bool ok = run_int(vm,
        "(begin (define big (make-vector 3000 0)) (fill-vectors! big 20)"
        " (let f ((i 0)) (if (< i 3000) (begin (vector-set! big i #f) (f (+ i 2)))))"
        " (define l (iota-list 20000))"
        " (let f ((i 0)) (if (< i 3000) (begin (vector-set! big i (make-vector 9 i)) (f (+ i 2)))))"
        " (define huge (make-vector 60000 7))"
        " (churn 5000)"
        " (+ (sum-list l) (sum-first big) (vector-ref huge 59999)))") ==
        (19999LL * 20000 / 2) + (2999LL * 3000 / 2) + 7;
    check("lists, small vectors and a 240 KB vector survive", ok, "");
    /* a list whose cells are interleaved with garbage, kept through many collections */
    ok = run_int(vm,
        "(begin (set! big #f) (set! huge #f) (set! l #f)"
        " (define keep '())"
        " (let f ((i 0)) (if (< i 12000) (begin (set! keep (cons i keep)) (make-vector 3 i) (f (+ i 1)))))"
        " (churn 20000)"
        " (define keep2 (iota-list 15000))"
        " (+ (sum-list keep) (sum-list keep2)))") == (11999LL * 12000 / 2) + (14999LL * 15000 / 2);
    check("interleaved cells survive", ok, "");
    /* drop everything: the heap must be usable again for one big object */
    ok = run_int(vm,
        "(begin (set! keep #f) (set! keep2 #f) (churn 100)"
        " (define huge (make-vector 150000 3)) (vector-ref huge 149999))") == 3;
    MiniStats st = stats(vm);
    char note[120];
    snprintf(note, sizeof(note), "%u minor, %u major (%u compacting)", (unsigned)st.minor_gcs,
             (unsigned)st.major_gcs, (unsigned)st.compact_gcs);
    check("a 600 KB vector fits after everything is dropped", ok, note);
    mini_destroy(vm);
}

/* 4. Old objects pointing at young ones across many collections (the remembered set) */
static void test_old_to_young(void) {
    printf("\n--- 4. Old-to-young pointers, mutation and explicit collections ---\n");
    MiniVM *vm = make_vm(512 * 1024, 4096);
    run(vm, PRELUDE);
    bool ok = run_int(vm,
        "(begin (define tbl (make-vector 2000 '())) (churn 200)"
        " (let round ((r 0)) (if (< r 60) (begin"
        "   (let f ((i 0)) (if (< i 2000) (begin"
        "      (vector-set! tbl i (if (> (length (vector-ref tbl i)) 6) (list r) (cons r (vector-ref tbl i))))"
        "      (f (+ i 1)))))"
        "   (round (+ r 1)))))"
        " (let f ((i 0) (s 0)) (if (< i 2000) (f (+ i 1) (+ s (car (vector-ref tbl i)))) s)))") == 59LL * 2000;
    check("table of growing lists stays consistent", ok, "");
    long long before = run_int(vm, "(sum-list (vector-ref tbl 7))");
    for (int i = 0; i < 5; i++) {
        mini_gc(vm);
        ok = ok && run_int(vm, "(begin (churn 300) (sum-list (vector-ref tbl 7)))") == before;
    }
    check("explicit collections between runs", ok, "");
    ok = run_int(vm,
        "(begin (define (deep n) (if (= n 0) '() (cons (make-vector 2 n) (deep (- n 1)))))"
        " (define d (deep 150)) (churn 3000)"
        " (let f ((l d) (s 0)) (if (null? l) s (f (cdr l) (+ s (vector-ref (car l) 0))))))") == 150LL * 151 / 2;
    check("data built on a deep stack", ok, "");
    ok = run_int(vm,
        "(begin (define ss (make-vector 300 \"\"))"
        " (let f ((i 0)) (if (< i 300) (begin (vector-set! ss i (string-append \"item-\" (number->string i))) (f (+ i 1)))))"
        " (churn 3000)"
        " (let f ((i 0) (s 0)) (if (< i 300) (f (+ i 1) (+ s (string-length (vector-ref ss i)))) s)))") ==
        5 * 300 + 10 + 90 * 2 + 200 * 3;
    check("strings", ok, "");
    mini_destroy(vm);
}

/* 5. Running out: an error, not a crash, and the VM works afterwards */
static void test_exhaustion(void) {
    printf("\n--- 5. Heap exhaustion is an error and the VM recovers ---\n");
    MiniVM *vm = make_vm(256 * 1024, 4096);
    run(vm, PRELUDE);
    Value r = run(vm, "(begin (define hog (iota-list 1000000)) 1)");
    (void)r;
    bool failed = vm->has_error;
    printf("    (the error above is expected)\n");
    mini_clear_error(vm);
    bool ok = run_int(vm, "(begin (churn 2000) (sum-list (iota-list 1000)))") == 999LL * 1000 / 2;
    check("out of memory reported, then normal work", failed && ok, "");
    r = run(vm, "(begin (define hog2 (make-vector 100000 0)) 1)");
    failed = vm->has_error;
    printf("    (the error above is expected)\n");
    mini_clear_error(vm);
    ok = run_int(vm, "(begin (define v (make-vector 20000 5)) (churn 2000) (vector-ref v 19999))") == 5;
    check("an object larger than the heap is refused", failed && ok, "");
    mini_destroy(vm);
}

/* 6. Long mixed run with a rolling window of live data of mixed sizes */
static void test_rolling_window(void) {
    printf("\n--- 6. Rolling window of mixed sizes ---\n");
    MiniVM *vm = make_vm(512 * 1024, 4096);
    run(vm, PRELUDE);
    bool ok = run_int(vm,
        "(begin (define win (make-vector 500 #f)) (define seed 12345)"
        " (define (rnd n) (set! seed (modulo (+ (* seed 1103) 12345) 65536)) (modulo seed n))"
        " (define (make-item i) (let ((k (rnd 4)))"
        "    (cond ((= k 0) (make-vector (+ 1 (rnd 60)) i))"
        "          ((= k 1) (cons i (iota-list (rnd 30))))"
        "          ((= k 2) (cons i (make-string (+ 1 (rnd 200)) #\\a)))"
        "          (else (make-vector (+ 100 (rnd 1500)) i)))))"
        " (define (item-id x) (if (vector? x) (vector-ref x 0) (car x)))"
        " (define bad 0)"
        " (let f ((i 0)) (if (< i 60000) (begin"
        "    (let* ((slot (rnd 500)) (old (vector-ref win slot)))"
        "      (if (and old (not (= (modulo (item-id old) 500) slot))) (set! bad (+ bad 1)))"
        "      (vector-set! win slot (make-item (+ slot (* 500 (rnd 100))))))"
        "    (f (+ i 1)))))"
        " (let f ((i 0)) (if (< i 500) (begin"
        "    (let ((x (vector-ref win i))) (if (and x (not (= (modulo (item-id x) 500) i))) (set! bad (+ bad 1))))"
        "    (f (+ i 1)))))"
        " bad)") == 0;
    MiniStats st = stats(vm);
    char note[120];
    snprintf(note, sizeof(note), "%u minor, %u major (%u compacting), %u MB allocated", (unsigned)st.minor_gcs,
             (unsigned)st.major_gcs, (unsigned)st.compact_gcs, (unsigned)(st.alloc_bytes >> 20));
    check("60000 replacements, every item checked", ok, note);
    mini_destroy(vm);
}

/* 7. A heap that starts small and grows to its limit */
static void test_growing_heap(void) {
    printf("\n--- 7. A heap growing from 64 KB to its limit ---\n");
    MiniConfig cfg;
    memset(&cfg, 0, sizeof(cfg));
    cfg.initial_heap_bytes = 64 * 1024;
    cfg.max_heap_bytes = 2u * 1024 * 1024 * (sizeof(Value) / 4);
    cfg.eden_bytes = 8192;
    cfg.skip_init_fasl = true;
    MiniVM *vm = mini_create_ext(&cfg);
    assert(vm != NULL);
    mini_register_hardware_lib(vm);
    Value init_code = msbc_deserialize(vm, init_msbc_data, init_msbc_len);
    mini_vm_run(vm, mini_make_closure(vm, init_code, mini_nil()), 0, NULL);
    run(vm, PRELUDE);
    bool ok = run_int(vm,
        "(begin (define keep (iota-list 40000)) (define big (make-vector 2000 0)) (fill-vectors! big 100)"
        " (churn 30000) (define more (iota-list 30000)) (churn 30000)"
        " (set! big #f) (churn 30000) (define v (make-vector 100000 2))"
        " (+ (quotient (sum-list more) 4) (quotient (sum-list keep) 8) (vector-ref v 99999)))") ==
        (29999LL * 30000 / 8) + (39999LL * 40000 / 16) + 2;
    MiniStats st = stats(vm);
    char note[140];
    snprintf(note, sizeof(note), "heap %u KB, %u minor, %u major (%u compacting)", (unsigned)(st.heap_bytes / 1024),
             (unsigned)st.minor_gcs, (unsigned)st.major_gcs, (unsigned)st.compact_gcs);
    check("data survives the growth and the churn after it", ok, note);
    mini_destroy(vm);
}

/* 8. Wide and deep structures: the mark stack and its overflow path */
static void test_wide_structures(void) {
    printf("\n--- 8. Wide and deep structures (mark stack) ---\n");
    MiniVM *vm = make_vm(2u * 1024 * 1024, 4096);
    run(vm, PRELUDE);
    bool ok = run_int(vm,
        "(begin"
        " (define wide (make-vector 20000 0))"
        " (let f ((i 0)) (if (< i 20000) (begin (vector-set! wide i (vector i (list i i))) (f (+ i 1)))))"
        " (define (tree d) (if (= d 0) 1 (vector (tree (- d 1)) (tree (- d 1)))))"
        " (define (count t) (if (vector? t) (+ (count (vector-ref t 0)) (count (vector-ref t 1))) t))"
        " (define tr (tree 12))"
        " (define lol (let loop ((i 0) (l '())) (if (= i 5000) l (loop (+ i 1) (cons (list i (vector i)) l)))))"
        " (churn 60000)"
        " (+ (count tr)"
        "    (let f ((i 0) (s 0)) (if (< i 20000) (f (+ i 1) (+ s (car (vector-ref (vector-ref wide i) 1)))) s))"
        "    (let f ((l lol) (s 0)) (if (null? l) s (f (cdr l) (+ s (vector-ref (cadr (car l)) 0)))))))") ==
        4096 + (19999LL * 20000 / 2) + (4999LL * 5000 / 2);
    for (int i = 0; i < 3 && ok; i++) {
        mini_gc(vm);
        ok = run_int(vm, "(begin (churn 2000) (count tr))") == 4096;
    }
    MiniStats st = stats(vm);
    char note[120];
    snprintf(note, sizeof(note), "%u minor, %u major (%u compacting)", (unsigned)st.minor_gcs,
             (unsigned)st.major_gcs, (unsigned)st.compact_gcs);
    check("wide vector, binary tree, list of lists", ok, note);
    mini_destroy(vm);
}

int main(void) {
    printf("========================================================================\n");
    printf("  MiniScheme collector suite (%u-bit values, old generation: %s)\n", (unsigned)(sizeof(Value) * 8),
#if defined(MINI_GC_MARKSWEEP) && MINI_GC_MARKSWEEP
           "mark-sweep"
#else
           "mark-compact"
#endif
    );
    printf("========================================================================\n");

    MiniConfig comp_cfg;
    memset(&comp_cfg, 0, sizeof(comp_cfg));
    comp_cfg.initial_heap_bytes = 1024 * 1024;
    comp_cfg.skip_init_fasl = true;
    g_comp = mini_create_ext(&comp_cfg);
    assert(g_comp != NULL);
    Value comp_code = msbc_deserialize(g_comp, compiler_msbc_data, compiler_msbc_len);
    assert(mini_is_code(comp_code));
    Value comp_clo = mini_make_closure(g_comp, comp_code, mini_nil());
    mini_vm_run(g_comp, comp_clo, 0, NULL);
    assert(!mini_get_error(g_comp));

    test_full_heap_churn();
    test_nearly_full_heap();
    test_fragmentation();
    test_old_to_young();
    test_exhaustion();
    test_rolling_window();
    test_growing_heap();
    test_wide_structures();

    mini_destroy(g_comp);
    printf("\n========================================================================\n");
    printf("  Collector suite: %d / %d PASSED\n", g_passed, g_total);
    printf("========================================================================\n");
    return g_passed == g_total ? 0 : 1;
}
