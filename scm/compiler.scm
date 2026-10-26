;;; =========================================================================
;;; compiler.scm - Ribbit-style Optimizing Scheme Bytecode Compiler
;;; =========================================================================
;;; Implements:
;;;   1. Complete R7RS S-Expression Reader (read-from-string)
;;;   2. Macro Expander (syntax-rules hygienic macros & define-macro)
;;;   3. Ribbit-Style AST Optimizer:
;;;      - Constant Folding: (+ 1 2) -> 3, (* 2 3) -> 6, (< 1 2) -> #t, etc.
;;;      - Branch Elimination: (if #t a b) -> a, (if #f a b) -> b
;;;      - Algebraic Simplification: (+ x 0) -> x, (* x 1) -> x, (- x 0) -> x
;;;   4. Bytecode Code Generator with Tail-Call Optimization (TCO)
;;;      - Direct Opcode Emission: OP_ADD, OP_SUB, OP_MUL, OP_LT, OP_CONS, etc.
;;;      - Immediate Integer Packing: OP_INT8 for -128..127
;;;      - Self-Tail-Call Optimization: OP_TAIL_CALL_SELF
;;;   5. eval, eval-string, eval-file
;;; =========================================================================

;; -------------------------------------------------------------------------
;; 1. Virtual Machine Opcodes
;; -------------------------------------------------------------------------

(define OP_HALT 0)
(define OP_CONST 1)
(define OP_INT8 2)
(define OP_NIL 3)
(define OP_TRUE 4)
(define OP_FALSE 5)
(define OP_VOID 6)
(define OP_GET_LOCAL 7)
(define OP_SET_LOCAL 8)
(define OP_GET_UPVALUE 9)
(define OP_SET_UPVALUE 10)
(define OP_GET_GLOBAL 11)
(define OP_SET_GLOBAL 12)
(define OP_DEF_GLOBAL 13)
(define OP_POP 14)
(define OP_JUMP 15)
(define OP_JUMP_IF_FALSE 16)
(define OP_CLOSURE 17)
(define OP_CALL 18)
(define OP_TAIL_CALL 19)
(define OP_RETURN 20)

;; -------------------------------------------------------------------------
;; 2. S-Expression Reader
;; -------------------------------------------------------------------------

(define (make-reader str)
  (cons str 0))

(define (reader-peek r)
  (let ((str (car r))
        (idx (cdr r)))
    (if (< idx (string-length str))
        (string-ref str idx)
        #f)))

(define (reader-get r)
  (let ((c (reader-peek r)))
    (if c (set-cdr! r (+ (cdr r) 1)))
    c))

(define (reader-unget r)
  (if (> (cdr r) 0)
      (set-cdr! r (- (cdr r) 1))))

;; The line (from 1) of byte idx, for error messages
(define (reader-line r idx)
  (let ((str (car r)))
    (let loop ((i 0) (n 1))
      (if (>= i idx) n (loop (+ i 1) (if (= (string-ref str i) 10) (+ n 1) n))))))

(define (reader-error r idx what)
  (error (string-append what " at line " (number->string (reader-line r idx)))))

(define (reader-whitespace? c)
  (or (= c 32) (= c 9) (= c 10) (= c 13)))

(define (reader-skip-comment r c2)
  (cond
    ((not c2) #f)
    ((or (= c2 10) (= c2 13))
     (reader-get r)
     (skip-whitespace r))
    (else
     (reader-get r)
     (reader-skip-comment r (reader-peek r)))))

(define (skip-whitespace r)
  (let ((c (reader-peek r)))
    (cond
      ((not c) #f)
      ((reader-whitespace? c)
       (reader-get r)
       (skip-whitespace r))
      ((= c 59) ; ';' comment till end-of-line
       (reader-get r)
       (reader-skip-comment r (reader-peek r)))
      (else #t))))

;; The rest of a list whose "(" is at byte open
(define (read-list r open)
  (skip-whitespace r)
  (let ((c (reader-peek r)))
    (cond
      ((not c) (reader-error r open "unexpected end of input: no ')' for the '('"))
      ((= c 41) ; ')'
       (reader-get r)
       '())
      ((= c 46) ; '.' dotted pair
       (reader-get r)
       (let ((next (reader-peek r)))
         (if (or (not next) (reader-whitespace? next) (= next 40) (= next 41) (= next 34) (= next 59))
             (begin
               (skip-whitespace r)
               (let ((tail (read-sexpr r)))
                 (skip-whitespace r)
                 (let ((close (reader-get r)))
                   (if (and close (= close 41))
                       tail
                       (reader-error r (- (cdr r) 1) "expected ')' after dotted pair tail")))))
             (begin
               (reader-unget r)
               (let* ((head (read-sexpr r))
                      (tail (read-list r open)))
                 (cons head tail))))))
      (else
       (let* ((head (read-sexpr r))
              (tail (read-list r open)))
         (cons head tail))))))

(define (read-string-literal r)
  (define open (cdr r))
  (reader-get r) ; consume opening '"'
  (define (iter-chars chars)
    (let ((c (reader-get r)))
      (cond
        ((not c) (reader-error r open "unexpected end of input: the string literal is not closed, it starts"))
        ((= c 34) ; '"'
         (list->string (reverse chars)))
        ((= c 92) ; '\' escape
         (let ((esc (reader-get r)))
           (cond
             ((= esc 110) (iter-chars (cons 10 chars))) ; \n
             ((= esc 116) (iter-chars (cons 9 chars)))  ; \t
             ((= esc 114) (iter-chars (cons 13 chars))) ; \r
             ((= esc 34)  (iter-chars (cons 34 chars))) ; \"
             ((= esc 92)  (iter-chars (cons 92 chars))) ; \\
             (else (iter-chars (cons esc chars))))))
        (else
         (iter-chars (cons c chars))))))
  (iter-chars '()))

(define (read-atom r)
  (define (iter-chars chars)
    (let ((c (reader-peek r)))
      (if (or (not c)
              (reader-whitespace? c)
              (= c 40)  ; (
              (= c 41)  ; )
              (= c 34)  ; "
              (= c 59)) ; ;
          (let* ((str (list->string (reverse chars)))
                 (num (string->number str)))
            (if num num (string->symbol str)))
          (begin
            (reader-get r)
            (iter-chars (cons c chars))))))
  (iter-chars '()))

(define (read-radix-number r radix)
  (define (iter-chars chars)
    (let ((c (reader-peek r)))
      (if (or (not c)
              (reader-whitespace? c)
              (= c 40)  ; (
              (= c 41)  ; )
              (= c 34)  ; "
              (= c 59)) ; ;
          (let* ((str (list->string (reverse chars)))
                 (num (string->number str radix)))
            (if num
                num
                (error "invalid numeric syntax")))
          (begin
            (reader-get r)
            (iter-chars (cons c chars))))))
  (iter-chars '()))

;; A character after #\ (R7RS): its first character is taken as it is, even
;; a delimiter (#\( #\) #\" #\; #\ ); letters and digits right behind a letter
;; make a name (#\space, #\x41). A UTF-8 character stays its first byte, as
;; before. Characters are their codes.
(define (char-delimiter? c)
  (or (reader-whitespace? c) (= c 40) (= c 41) (= c 34) (= c 59)))

(define (ascii-letter? c) (or (and (>= c 65) (<= c 90)) (and (>= c 97) (<= c 122))))

(define char-names
  '((newline . 10) (space . 32) (tab . 9) (return . 13) (linefeed . 10) (nul . 0) (null . 0)
    (alarm . 7) (backspace . 8) (escape . 27) (delete . 127)))

(define (read-char-literal r)
  (let ((first (reader-get r)))
    (cond
      ((not first) (reader-error r (cdr r) "unexpected end of input after #\\"))
      ((>= first 128)
       ;; the rest of a UTF-8 sequence (continuation bytes 128..191) is dropped
       (let skip ()
         (let ((c (reader-peek r)))
           (if (and c (>= c 128) (< c 192)) (begin (reader-get r) (skip)))))
       first)
      ((not (ascii-letter? first)) first)
      (else
       (let loop ((chars (list first)))
         (let ((c (reader-peek r)))
           (if (and c (not (char-delimiter? c)))
               (begin (reader-get r) (loop (cons c chars)))
               (if (null? (cdr chars))
                   first
                   (let* ((codes (map (lambda (k) (if (and (>= k 65) (<= k 90)) (+ k 32) k)) (reverse chars)))
                          (name (list->string codes))
                          (named (assq (string->symbol name) char-names))
                          (hex (and (= (car codes) 120)
                                    (string->number (substring name 1 (string-length name)) 16))))
                     (cond (named (cdr named))
                           ((and hex (>= hex 0)) hex)
                           (else (error (string-append "unknown character name: #\\" name)))))))))))))

(define (read-sharp r)
  (define open (cdr r))
  (reader-get r) ; consume '#'
  (let ((c (reader-get r)))
    (cond
      ((not c) (reader-error r open "unexpected end of input after #"))
      ((or (= c 116) (= c 84)) #t) ; #t or #T
      ((or (= c 102) (= c 70)) #f) ; #f or #F
      ((= c 40)      ; #( vector
       (list->vector (read-list r open)))
      ((= c 92)      ; #\ character
       (read-char-literal r))
      ((or (= c 120) (= c 88)) ; #x or #X (hexadecimal)
       (read-radix-number r 16))
      ((or (= c 98) (= c 66))  ; #b or #B (binary)
       (read-radix-number r 2))
      ((or (= c 111) (= c 79)) ; #o or #O (octal)
       (read-radix-number r 8))
      ((or (= c 100) (= c 68)) ; #d or #D (decimal)
       (read-radix-number r 10))
      (else
       (reader-error r open "unknown sharp syntax")))))

(define (read-sexpr r)
  (skip-whitespace r)
  (let ((c (reader-peek r)))
    (cond
      ((not c) (eof-object))
      ((= c 40) (reader-get r) (read-list r (- (cdr r) 1)))
      ;; a ")" no "(" opened: an error (read-atom would make it an empty symbol and stop
      ;; nowhere, read-from-string asking for the next expression at the same ")" forever)
      ((= c 41) (reader-error r (cdr r) "unexpected ')' with no '(' to close"))
      ((= c 39) (reader-get r) (list 'quote (read-sexpr r)))
      ((= c 96) (reader-get r) (list 'quasiquote (read-sexpr r)))
      ((= c 44) ; ','
       (reader-get r)
       (if (and (reader-peek r) (= (reader-peek r) 64)) ; '@'
           (begin (reader-get r) (list 'unquote-splicing (read-sexpr r)))
           (list 'unquote (read-sexpr r))))
      ((= c 34) (read-string-literal r))
      ((= c 35) (read-sharp r))
      (else (read-atom r)))))

(define (read-from-string str)
  (let ((r (make-reader str)))
    (define (iter-acc acc)
      (skip-whitespace r)
      (if (not (reader-peek r))
          (reverse acc)
          (iter-acc (cons (read-sexpr r) acc))))
    (iter-acc '())))

;; -------------------------------------------------------------------------
;; 3. Macro Expander (syntax-rules & define-macro)
;; -------------------------------------------------------------------------

(define *macro-table* (if (%defined? '*macro-table*) *macro-table* '()))

(define (macro-lookup name)
  (let ((entry (assq name *macro-table*)))
    (if entry (cdr entry) #f)))

(define (macro-register! name expander)
  (set! *macro-table* (cons (cons name expander) *macro-table*)))

(define *gensym-counter* 0)
(define (gensym . opt-prefix)
  (set! *gensym-counter* (+ *gensym-counter* 1))
  (string->symbol (string-append (if (null? opt-prefix) "g" (car opt-prefix))
                                 "__g"
                                 (number->string *gensym-counter*))))

(define *special-forms*
  '(if begin lambda quote define set! let let* letrec define-syntax define-macro syntax-rules ... _))

;; Pattern matching for syntax-rules
(define (syntax-match-ellipsis sub-pat rest-pat e literals bindings matched-list)
  (let ((rest-bindings (syntax-match-pattern rest-pat e literals bindings)))
    (if rest-bindings
        ;; Match succeeded for rest; package matched-list for ellipsis vars
        (let ((grouped (syntax-group-ellipsis sub-pat (reverse matched-list))))
          (append grouped rest-bindings))
        (if (pair? e)
            (let ((sub-b (syntax-match-pattern sub-pat (car e) literals '())))
              (if sub-b
                  (syntax-match-ellipsis sub-pat rest-pat (cdr e) literals bindings (cons sub-b matched-list))
                  #f))
            #f))))

(define (syntax-match-pattern pat expr literals bindings)
  (cond
    ((not bindings) #f)
    ((symbol? pat)
     (if (memq pat literals)
         (and (symbol? expr) (eq? pat expr) bindings)
         (cons (cons pat expr) bindings)))
    ((null? pat)
     (and (null? expr) bindings))
    ((pair? pat)
     (if (and (pair? (cdr pat)) (eq? (cadr pat) '...))
         ;; (sub-pat ... . rest-pat)
         (syntax-match-ellipsis (car pat) (cddr pat) expr literals bindings '())
         (and (pair? expr)
              (let ((b1 (syntax-match-pattern (car pat) (car expr) literals bindings)))
                (and b1 (syntax-match-pattern (cdr pat) (cdr expr) literals b1))))))
    (else
     (and (equal? pat expr) bindings))))

(define (syntax-group-ellipsis pat matches)
  (let ((vars (syntax-collect-pattern-vars pat '())))
    (map (lambda (v)
           (cons v (map (lambda (m)
                          (let ((entry (assq v m)))
                            (if entry (cdr entry) #f)))
                        matches)))
         vars)))

(define (syntax-collect-pattern-vars pat acc)
  (cond
    ((symbol? pat) (if (eq? pat '...) acc (cons pat acc)))
    ((pair? pat)
     (syntax-collect-pattern-vars (car pat)
       (syntax-collect-pattern-vars (cdr pat) acc)))
    (else acc)))

(define (syntax-expand-rows i max-count vars sub-tmpl rest-tmpl bindings literals gensym-env res)
  (if (< i max-count)
      (let ((row-bindings
             (map (lambda (v)
                    (let ((entry (assq v bindings)))
                      (if (and entry (pair? (cdr entry)) (> (length (cdr entry)) i))
                          (cons v (list-ref (cdr entry) i))
                          (cons v #f))))
                  vars)))
        (syntax-expand-rows (+ i 1) max-count vars sub-tmpl rest-tmpl bindings literals gensym-env
                            (cons (syntax-expand-template sub-tmpl (append row-bindings bindings) literals gensym-env) res)))
      (append (reverse res) (syntax-expand-template rest-tmpl bindings literals gensym-env))))

;; Template expansion for syntax-rules with hygiene
(define (syntax-expand-template tmpl bindings literals gensym-env)
  (cond
    ((symbol? tmpl)
     (let ((entry (assq tmpl bindings)))
       (cond
         (entry (cdr entry))
         ((memq tmpl literals) tmpl)
         ((memq tmpl *special-forms*) tmpl)
         ((%defined? tmpl) tmpl)
         (else
          (let ((g-entry (assq tmpl (car gensym-env))))
            (if g-entry
                (cdr g-entry)
                (let ((g (gensym (symbol->string tmpl))))
                  (set-car! gensym-env (cons (cons tmpl g) (car gensym-env)))
                  g)))))))
    ((pair? tmpl)
     (if (and (pair? (cdr tmpl)) (eq? (cadr tmpl) '...))
         ;; (sub-tmpl ... . rest-tmpl)
         (let* ((sub-tmpl (car tmpl))
                (rest-tmpl (cddr tmpl))
                (vars (syntax-collect-pattern-vars sub-tmpl '()))
                ;; Find length of matches
                (counts (map (lambda (v)
                               (let ((entry (assq v bindings)))
                                 (if (and entry (pair? (cdr entry)))
                                     (length (cdr entry))
                                     0)))
                             vars))
                (max-count (if (null? counts) 0 (apply max counts))))
           (syntax-expand-rows 0 max-count vars sub-tmpl rest-tmpl bindings literals gensym-env '()))
         (cons (syntax-expand-template (car tmpl) bindings literals gensym-env)
               (syntax-expand-template (cdr tmpl) bindings literals gensym-env))))
    (else tmpl)))

(define (make-syntax-rules-expander literals rules)
  (lambda (form)
    (define (match-rules r)
      (if (null? r)
          (error "no syntax-rules pattern matched")
          (let* ((rule (car r))
                 (pat (cdar rule)) ; strip keyword from pattern head
                 (tmpl (cadr rule))
                 (args (cdr form))
                 (bindings (syntax-match-pattern pat args literals '())))
            (if bindings
                (let ((gensym-env (list '())))
                  (syntax-expand-template tmpl bindings literals gensym-env))
                (match-rules (cdr r))))))
    (match-rules rules)))

(define (macro-expand-1 expr)
  (if (and (pair? expr) (symbol? (car expr)))
      (let ((op (car expr)))
        (let ((expander (macro-lookup op)))
          (if expander
              (cons #t (expander expr))
              (cons #f expr))))
      (cons #f expr)))

(define (macro-expand expr)
  (let ((res (macro-expand-1 expr)))
    (if (car res)
        (macro-expand (cdr res))
        (if (pair? expr)
            (cons (macro-expand (car expr))
                  (if (pair? (cdr expr))
                      (macro-expand-list (cdr expr))
                      (macro-expand (cdr expr))))
            expr))))

(define (macro-expand-list lst)
  (if (null? lst)
      '()
      (cons (macro-expand (car lst))
            (if (pair? (cdr lst))
                (macro-expand-list (cdr lst))
                (macro-expand (cdr lst))))))

;; -------------------------------------------------------------------------
;; 4. Ribbit-Style AST Optimizer
;; -------------------------------------------------------------------------

(define (optimize-ast expr)
  (cond
    ((not (pair? expr)) expr)
    ((eq? (car expr) 'quote) expr)

    ;; Branch Elimination: (if test then [else])
    ((eq? (car expr) 'if)
     (let* ((c (optimize-ast (cadr expr)))
            (t (optimize-ast (caddr expr)))
            (e (if (pair? (cdddr expr)) (optimize-ast (cadddr expr)) (void))))
       (cond
         ((and (boolean? c) (not c)) e) ; (if #f t e) -> e
         ((or (number? c) (string? c) (and (boolean? c) c) (null? c)) t) ; (if truthy t e) -> t
         (else (list 'if c t e)))))

    ;; Constant folding of '+', '-' and '*' on two number literals. No
    ;; identities such as (+ x 0) -> x or (* x 0) -> 0: they would skip the
    ;; type check ((* 'a 1) must raise) and lose inexactness ((* 2.5 0) is 0.0)
    ((eq? (car expr) '+)
     (let ((args (map optimize-ast (cdr expr))))
       (if (and (= (length args) 2) (number? (car args)) (number? (cadr args)))
           (+ (car args) (cadr args))
           (cons '+ args))))

    ((eq? (car expr) '-)
     (let ((args (map optimize-ast (cdr expr))))
       (if (and (= (length args) 2) (number? (car args)) (number? (cadr args)))
           (- (car args) (cadr args))
           (cons '- args))))

    ((eq? (car expr) '*)
     (let ((args (map optimize-ast (cdr expr))))
       (if (and (= (length args) 2) (number? (car args)) (number? (cadr args)))
           (* (car args) (cadr args))
           (cons '* args))))

    ;; Constant Folding for Comparison Predicates
    ((eq? (car expr) '<)
     (let ((args (map optimize-ast (cdr expr))))
       (if (and (= (length args) 2) (number? (car args)) (number? (cadr args)))
           (< (car args) (cadr args))
           (cons '< args))))
    ((eq? (car expr) '<=)
     (let ((args (map optimize-ast (cdr expr))))
       (if (and (= (length args) 2) (number? (car args)) (number? (cadr args)))
           (<= (car args) (cadr args))
           (cons '<= args))))
    ((eq? (car expr) '>)
     (let ((args (map optimize-ast (cdr expr))))
       (if (and (= (length args) 2) (number? (car args)) (number? (cadr args)))
           (> (car args) (cadr args))
           (cons '> args))))
    ((eq? (car expr) '>=)
     (let ((args (map optimize-ast (cdr expr))))
       (if (and (= (length args) 2) (number? (car args)) (number? (cadr args)))
           (>= (car args) (cadr args))
           (cons '>= args))))
    ((eq? (car expr) '=)
     (let ((args (map optimize-ast (cdr expr))))
       (if (and (= (length args) 2) (number? (car args)) (number? (cadr args)))
           (= (car args) (cadr args))
           (cons '= args))))

    ;; Constant Folding for 'not'
    ((and (eq? (car expr) 'not) (pair? (cdr expr)) (null? (cddr expr)))   ; (not x) only: other arities raise at run time
     (let ((arg (optimize-ast (cadr expr))))
       (cond
         ((and (boolean? arg) (not arg)) #t)
         ((or (number? arg) (string? arg) (and (boolean? arg) arg) (null? arg)) #f)
         (else (list 'not arg)))))

    ;; Recurse into forms
    ((eq? (car expr) 'begin)
     (cons 'begin (map optimize-ast (cdr expr))))
    ((eq? (car expr) 'lambda)
     (list 'lambda (cadr expr) (optimize-ast (if (null? (cddr expr)) (void) (if (null? (cdddr expr)) (caddr expr) (cons 'begin (cddr expr)))))))
    ((eq? (car expr) 'define)
     (if (pair? (cadr expr))
         (cons 'define (cons (cadr expr) (map optimize-ast (cddr expr))))
         (list 'define (cadr expr) (optimize-ast (caddr expr)))))
    ((eq? (car expr) 'set!)
     (list 'set! (cadr expr) (optimize-ast (caddr expr))))
    (else
     (let loop ((l expr))
       (cond
         ((null? l) '())
         ((pair? l) (cons (optimize-ast (car l)) (loop (cdr l))))
         (else l))))))

;; -------------------------------------------------------------------------
;; 5. Bytecode Compiler & Code Generator
;; -------------------------------------------------------------------------

(define (make-compiler-ctx parent fn-name)
  (list parent        ; 0: parent
        fn-name       ; 1: fn-name
        '()           ; 2: locals: list of (sym . slot)
        0             ; 3: num-locals
        0             ; 4: max-locals
        0             ; 5: num-params
        #f            ; 6: is-variadic
        (cons #f #f)  ; 7: has-inner-lambdas box (car)
        '()           ; 8: consts
        '()))         ; 9: code (bytes in reverse)

(define (ctx-parent ctx) (list-ref ctx 0))
(define (ctx-fn-name ctx) (list-ref ctx 1))
(define (ctx-locals ctx) (list-ref ctx 2))
(define (set-ctx-locals! ctx v) (list-set! ctx 2 v))
(define (ctx-num-locals ctx) (list-ref ctx 3))
(define (set-ctx-num-locals! ctx v) (list-set! ctx 3 v))
(define (ctx-max-locals ctx) (list-ref ctx 4))
(define (set-ctx-max-locals! ctx v) (list-set! ctx 4 v))
(define (ctx-num-params ctx) (list-ref ctx 5))
(define (set-ctx-num-params! ctx v) (list-set! ctx 5 v))
(define (ctx-is-variadic ctx) (list-ref ctx 6))
(define (set-ctx-is-variadic! ctx v) (list-set! ctx 6 v))
(define (ctx-has-inner-lambdas ctx) (car (list-ref ctx 7)))
(define (set-ctx-has-inner-lambdas! ctx v) (set-car! (list-ref ctx 7) v))
(define (ctx-consts ctx) (list-ref ctx 8))
(define (set-ctx-consts! ctx v) (list-set! ctx 8 v))
(define (ctx-code ctx) (list-ref ctx 9))
(define (set-ctx-code! ctx v) (list-set! ctx 9 v))

(define (emit-byte ctx b)
  (set-ctx-code! ctx (cons b (ctx-code ctx))))

(define (emit-u16 ctx n)
  (emit-byte ctx (modulo n 256))
  (emit-byte ctx (quotient n 256)))

(define (emit-int16 ctx n)
  (let ((val (if (< n 0) (+ 65536 n) n)))
    (emit-u16 ctx val)))

(define (add-const ctx val)
  (define (search lst idx)
    (cond
      ((null? lst)
       (set-ctx-consts! ctx (append (ctx-consts ctx) (list val)))
       idx)
      ((equal? (car lst) val) idx)
      (else (search (cdr lst) (+ idx 1)))))
  (search (ctx-consts ctx) 0))

(define (add-local! ctx sym)
  (let ((slot (ctx-num-locals ctx)))
    (set-ctx-locals! ctx (cons (cons sym slot) (ctx-locals ctx)))
    (set-ctx-num-locals! ctx (+ slot 1))
    (if (> (+ slot 1) (ctx-max-locals ctx))
        (set-ctx-max-locals! ctx (+ slot 1)))
    slot))

;; A plain let inside a procedure binds fresh slots of the enclosing frame
;; instead of calling an immediately-applied lambda: no closure, no extra
;; call frame and, unless a real lambda captures something, no heap
;; environment for the function. Slots are never reused, so a closure that
;; captured one keeps seeing only its own binding. Loops are calls, so each
;; iteration still gets a fresh frame.
(define (alloc-slot! ctx)
  (let ((slot (ctx-num-locals ctx)))
    (set-ctx-num-locals! ctx (+ slot 1))
    (if (> (+ slot 1) (ctx-max-locals ctx))
        (set-ctx-max-locals! ctx (+ slot 1)))
    slot))

(define (let-slots-ok? ctx vars body)
  (define (has-define? b)
    (and (pair? b)
         (or (and (pair? (car b)) (memq (caar b) '(define define-syntax define-macro)))
             (has-define? (cdr b)))))
  (and (ctx-parent ctx)
       (pair? vars)
       (< (+ (ctx-num-locals ctx) (length vars)) 200)
       (not (has-define? body))))

(define (compile-let-slots ctx vars vals body is-tail)
  ;; values are computed in the outer scope, then bound all at once
  (for-each (lambda (v) (compile-expr ctx v #f)) vals)
  (let* ((saved (ctx-locals ctx))
         (slots (map (lambda (v) (alloc-slot! ctx)) vars)))
    (for-each (lambda (slot)
                (emit-byte ctx OP_SET_LOCAL)
                (emit-byte ctx slot)
                (emit-byte ctx OP_POP))
              (reverse slots))
    (set-ctx-locals! ctx (append (map cons vars slots) saved))
    (compile-sequence ctx body is-tail)
    (set-ctx-locals! ctx saved)))

(define (lookup-var ctx sym)
  (let ((local (assq sym (ctx-locals ctx))))
    (if local
        (list 'local (cdr local))
        (let ()
          (define (search-parent p depth)
            (if (not p)
                (list 'global sym)
                (let ((plocal (assq sym (ctx-locals p))))
                  (if plocal
                      (let ()
                        ;; mark ancestors
                        (define (mark curr)
                          (set-ctx-has-inner-lambdas! curr #t)
                          (if (not (eq? curr p))
                              (mark (ctx-parent curr))))
                        (mark (ctx-parent ctx))
                        (list 'upvalue depth (cdr plocal)))
                      (search-parent (ctx-parent p) (+ depth 1))))))
          (search-parent (ctx-parent ctx) 1)))))

(define (patch-jump! ctx pos)
  (let* ((code (ctx-code ctx))
         (total-len (length code))
         ;; pos is the 0-based offset from start of code array where 2-byte operand begins
         (offset (- total-len (+ pos 2)))
         (off16 (if (< offset 0) (+ 65536 offset) offset))
         (b0 (modulo off16 256))
         (b1 (quotient off16 256))
         ;; code is stored in reverse: index 0 is the last emitted byte
         (rev-idx0 (- total-len 1 pos))
         (rev-idx1 (- total-len 1 (+ pos 1))))
    (list-set! code rev-idx0 b0)
    (list-set! code rev-idx1 b1)))

;; Helper: mutate list element
(define (list-set! lst idx val)
  (define (iter l i)
    (if (= i 0)
        (set-car! l val)
        (iter (cdr l) (- i 1))))
  (iter lst idx))

;; Main compiler dispatch
(define (compile-expr ctx expr is-tail)
  (cond
    ;; Constant / Literal: Small Integer (exact)
    ((and (exact? expr) (integer? expr) (>= expr -128) (<= expr 127))
     (emit-byte ctx OP_INT8)
     (emit-byte ctx (if (< expr 0) (+ 256 expr) expr))
     (if (and is-tail (ctx-parent ctx)) (emit-byte ctx OP_RETURN)))

    ;; Constant / Literal: Boolean
    ((boolean? expr)
     (emit-byte ctx (if expr OP_TRUE OP_FALSE))
     (if (and is-tail (ctx-parent ctx)) (emit-byte ctx OP_RETURN)))

    ;; Constant / Literal: Nil
    ((null? expr)
     (emit-byte ctx OP_NIL)
     (if (and is-tail (ctx-parent ctx)) (emit-byte ctx OP_RETURN)))

    ;; Constant / Literal: Void
    ((void? expr)
     (emit-byte ctx OP_VOID)
     (if (and is-tail (ctx-parent ctx)) (emit-byte ctx OP_RETURN)))

    ;; Constant / Literal: Other Numbers, Strings, Vectors
    ((or (number? expr) (string? expr) (vector? expr))
     (let ((idx (add-const ctx expr)))
       (emit-byte ctx OP_CONST)
       (emit-u16 ctx idx)
       (if (and is-tail (ctx-parent ctx)) (emit-byte ctx OP_RETURN))))

    ;; Variable Reference
    ((symbol? expr)
     (let ((var-info (lookup-var ctx expr)))
       (cond
         ((eq? (car var-info) 'local)
          (emit-byte ctx OP_GET_LOCAL)
          (emit-byte ctx (cadr var-info)))
         ((eq? (car var-info) 'upvalue)
          (emit-byte ctx OP_GET_UPVALUE)
          (emit-byte ctx (cadr var-info))
          (emit-byte ctx (caddr var-info)))
         ((eq? (car var-info) 'global)
          (let ((idx (add-const ctx (cadr var-info))))
            (emit-byte ctx OP_GET_GLOBAL)
            (emit-u16 ctx idx))))
       (if (and is-tail (ctx-parent ctx)) (emit-byte ctx OP_RETURN))))

    ;; Pair: Special Forms or Application
    ((pair? expr)
     (let ((op (car expr))
           (args (cdr expr)))
       (cond
         ;; (quote datum)
         ((eq? op 'quote)
          (let ((idx (add-const ctx (car args))))
            (emit-byte ctx OP_CONST)
            (emit-u16 ctx idx)
            (if (and is-tail (ctx-parent ctx)) (emit-byte ctx OP_RETURN))))

         ;; (include-bytes "path"): the file's bytes as a string constant,
         ;; read at compile time (fonts embedded into an app image)
         ((eq? op 'include-bytes)
          (let ((data (%read-file-string (car args))))
            (if (not data) (error "include-bytes: cannot read file" (car args)))
            (let ((idx (add-const ctx data)))
              (emit-byte ctx OP_CONST)
              (emit-u16 ctx idx)
              (if (and is-tail (ctx-parent ctx)) (emit-byte ctx OP_RETURN)))))

         ;; (if test then [else])
         ((eq? op 'if)
          (let ((test-expr (car args))
                (then-expr (cadr args))
                (else-expr (if (pair? (cddr args)) (caddr args) (void))))
            (compile-expr ctx test-expr #f)
            (emit-byte ctx OP_JUMP_IF_FALSE)
            (let ((jump-false-pos (length (ctx-code ctx))))
              (emit-u16 ctx 0)
              (compile-expr ctx then-expr is-tail)
              (emit-byte ctx OP_JUMP)
              (let ((jump-end-pos (length (ctx-code ctx))))
                (emit-u16 ctx 0)
                (patch-jump! ctx jump-false-pos)
                (compile-expr ctx else-expr is-tail)
                (patch-jump! ctx jump-end-pos)))))

         ;; (and e ...)
         ((eq? op 'and)
          (cond
            ((null? args) (compile-expr ctx #t is-tail))
            ((null? (cdr args)) (compile-expr ctx (car args) is-tail))
            (else
             (compile-expr ctx
               (list 'if (car args) (cons 'and (cdr args)) #f)
               is-tail))))

         ;; (or e ...)
         ((eq? op 'or)
          (cond
            ((null? args) (compile-expr ctx #f is-tail))
            ((null? (cdr args)) (compile-expr ctx (car args) is-tail))
            (else
             (let ((tmp (gensym "or")))
               (compile-expr ctx
                 (list 'let (list (list tmp (car args)))
                   (list 'if tmp tmp (cons 'or (cdr args))))
                 is-tail)))))

         ;; (begin expr ...)
         ((eq? op 'begin)
          (compile-sequence ctx args is-tail))

         ;; (define var val) / (define (name args...) body...)
         ((eq? op 'define)
          (let* ((target (car args))
                 (sym (if (pair? target) (car target) target))
                 (val (if (pair? target)
                          (cons 'lambda (cons (cdr target) (cdr args)))
                          (cadr args))))
            (let ((local (assq sym (ctx-locals ctx))))
              (if local
                  (begin
                    (compile-expr ctx val #f)
                    (emit-byte ctx OP_SET_LOCAL)
                    (emit-byte ctx (cdr local))
                    (emit-byte ctx OP_POP)
                    (emit-byte ctx OP_VOID))
                  (let ((idx (add-const ctx sym)))
                    (compile-expr ctx val #f)
                    (emit-byte ctx OP_DEF_GLOBAL)
                    (emit-u16 ctx idx))))
            (if (and is-tail (ctx-parent ctx)) (emit-byte ctx OP_RETURN))))

         ;; (set! var val)
         ((eq? op 'set!)
          (let* ((var (car args))
                 (val (cadr args))
                 (var-info (lookup-var ctx var)))
            (compile-expr ctx val #f)
            (cond
              ((eq? (car var-info) 'local)
               (emit-byte ctx OP_SET_LOCAL)
               (emit-byte ctx (cadr var-info)))
              ((eq? (car var-info) 'upvalue)
               (emit-byte ctx OP_SET_UPVALUE)
               (emit-byte ctx (cadr var-info))
               (emit-byte ctx (caddr var-info)))
              ((eq? (car var-info) 'global)
               (let ((idx (add-const ctx (cadr var-info))))
                 (emit-byte ctx OP_SET_GLOBAL)
                 (emit-u16 ctx idx))))
            (emit-byte ctx OP_POP)
            (emit-byte ctx OP_VOID)
            (if (and is-tail (ctx-parent ctx)) (emit-byte ctx OP_RETURN))))

         ;; (let ((v e) ...) body ...) or (let name ((v e) ...) body ...)
         ((eq? op 'let)
          (if (symbol? (car args))
              ;; Named let: (let name ((var val) ...) body ...)
              (let* ((name (car args))
                     (bindings (cadr args))
                     (body (cddr args))
                     (vars (map car bindings))
                     (vals (map cadr bindings)))
                (compile-expr ctx
                  (list 'letrec (list (list name (cons 'lambda (cons vars body))))
                        (cons name vals))
                  is-tail))
              ;; Standard let: (let ((var val) ...) body ...)
              (let* ((bindings (car args))
                     (body (cdr args))
                     (vars (map car bindings))
                     (vals (map cadr bindings)))
                (if (let-slots-ok? ctx vars body)
                    (compile-let-slots ctx vars vals body is-tail)
                    (compile-expr ctx
                      (cons (cons 'lambda (cons vars body)) vals)
                      is-tail)))))

         ;; (let* ((v e) ...) body ...)
         ((eq? op 'let*)
          (let* ((bindings (car args))
                 (body (cdr args)))
            (if (null? bindings)
                (compile-expr ctx (cons 'begin body) is-tail)
                (compile-expr ctx
                  (list 'let (list (car bindings))
                    (cons 'let* (cons (cdr bindings) body)))
                  is-tail))))

         ;; (letrec ((v e) ...) body ...)
         ((eq? op 'letrec)
          (let* ((bindings (car args))
                 (body (cdr args))
                 (vars (map car bindings))
                 (vals (map cadr bindings)))
            (define (make-set-list vs es)
              (if (null? vs)
                  '()
                  (cons (list 'set! (car vs) (car es))
                        (make-set-list (cdr vs) (cdr es)))))
            (compile-expr ctx
              (cons 'let
                (cons (map (lambda (v) (list v (list 'void))) vars)
                  (append (make-set-list vars vals)
                          body)))
              is-tail)))

         ;; (lambda params body ...)
         ((eq? op 'lambda)
          (let* ((params (car args))
                 (body (cdr args))
                 (child (make-compiler-ctx ctx #f)))
            ;; Scan and bind parameters
            (define (scan-params p p-idx)
              (cond
                ((null? p)
                 (set-ctx-num-params! child p-idx))
                ((symbol? p)
                 (add-local! child p)
                 (set-ctx-num-params! child (+ p-idx 1))
                 (set-ctx-is-variadic! child #t))
                ((pair? p)
                 (add-local! child (car p))
                 (scan-params (cdr p) (+ p-idx 1)))))
            (scan-params params 0)
            ;; Scan for internal defines
            (define (scan-defines b)
              (if (pair? b)
                  (let ((form (car b)))
                    (if (and (pair? form) (eq? (car form) 'define))
                        (let* ((target (cadr form))
                               (sym (if (pair? target) (car target) target)))
                          (add-local! child sym)))
                    (scan-defines (cdr b)))))
            (scan-defines body)
            ;; Compile body
            (compile-sequence child body #t)
            (let ((code (ctx-code child)))
              (if (or (null? code) (not (= (car code) OP_RETURN)))
                  (emit-byte child OP_RETURN)))
            ;; Create CodeObj
            (let* ((bytes (list->vector (reverse (ctx-code child))))
                   (consts (list->vector (ctx-consts child)))
                   (num-params (ctx-num-params child))
                   (max-stack (+ (ctx-max-locals child) 64))
                   (is-varargs (ctx-is-variadic child))
                   (has-inner (ctx-has-inner-lambdas child))
                   (co (%make-code bytes consts num-params max-stack is-varargs has-inner))
                   (idx (add-const ctx co)))
              (emit-byte ctx OP_CLOSURE)
              (emit-u16 ctx idx)
              (if (and is-tail (ctx-parent ctx)) (emit-byte ctx OP_RETURN)))))

         ;; General Procedure Call
         (else
          (compile-expr ctx op #f)
          (for-each (lambda (arg) (compile-expr ctx arg #f)) args)
          (let ((argc (length args)))
            (if is-tail
                (begin (emit-byte ctx OP_TAIL_CALL) (emit-byte ctx argc))
                (begin (emit-byte ctx OP_CALL) (emit-byte ctx argc))))))))))

(define (compile-sequence ctx seq is-tail)
  (cond
    ((null? seq)
     (emit-byte ctx OP_VOID)
     (if (and is-tail (ctx-parent ctx)) (emit-byte ctx OP_RETURN)))
    ((null? (cdr seq))
     (compile-expr ctx (car seq) is-tail))
    (else
     (compile-expr ctx (car seq) #f)
     (emit-byte ctx OP_POP)
     (compile-sequence ctx (cdr seq) is-tail))))

;; -------------------------------------------------------------------------
;; 6. Top-Level eval, eval-string, eval-file
;; -------------------------------------------------------------------------

(define (compile-to-code expr)
  (let ((ctx (make-compiler-ctx #f #f)))
    (compile-expr ctx expr #t)
    (let ((code (ctx-code ctx)))
      (if (or (null? code) (not (= (car code) OP_RETURN)))
          (emit-byte ctx OP_RETURN)))
    (let* ((bytes (list->vector (reverse (ctx-code ctx))))
           (consts (list->vector (ctx-consts ctx)))
           (num-params (ctx-num-params ctx))
           (max-stack (+ (ctx-max-locals ctx) 64))
           (is-varargs (ctx-is-variadic ctx))
           (has-inner (ctx-has-inner-lambdas ctx)))
      (%make-code bytes consts num-params max-stack is-varargs has-inner))))

(define (compile-one expr)
  (cond
    ((and (pair? expr) (eq? (car expr) 'define-macro))
     (lambda () (eval expr)))
    ((and (pair? expr) (eq? (car expr) 'define-syntax))
     (lambda () (eval expr)))
    (else
     (let* ((expanded (macro-expand expr))
            (optimized (optimize-ast expanded))
            (co (compile-to-code optimized)))
       (%make-closure co '())))))

(define (eval expr)
  ;; Check for macro definitions first
  (cond
    ((and (pair? expr) (eq? (car expr) 'define-macro))
     (let* ((sig (cadr expr))
            (body (cddr expr))
            (is-form (pair? sig))
            (name (if is-form (car sig) sig))
            (proc (if is-form
                      (let ((params (cdr sig)))
                        (eval (list 'lambda params (if (null? (cdr body)) (car body) (cons 'begin body)))))
                      (eval (car body)))))
       (let ((m (%make-macro #f proc '())))
         (%set-global! name m)
         (macro-register! name (lambda (form) (apply proc (cdr form))))
         name)))

    ((and (pair? expr) (eq? (car expr) 'define-syntax))
     (let* ((name (cadr expr))
            (sr-spec (caddr expr))
            (literals (cadr sr-spec))
            (rules (cddr sr-spec))
            (m (%make-macro #t literals rules)))
       (%set-global! name m)
       (macro-register! name (make-syntax-rules-expander literals rules))
       name))

    (else
     (let* ((expanded (macro-expand expr))
            (optimized (optimize-ast expanded))
            (co (compile-to-code optimized)))
       (%execute (%make-closure co '()))))))

(define (eval-string str)
  (let loop ((es (read-from-string str)) (res (void)))
    (if (null? es)
        res
        (let* ((e (car es))
               (rest (cdr es))
               (clo (compile-one e)))
          (set! es #f)
          (set! e #f)
          (let ((val (clo)))
            (set! clo #f)
            (loop rest val))))))

(define (eval-file filepath)
  (let ((content (%read-file-string filepath)))
    (if content
        (eval-string content)
        (error (string-append "cannot open file: " filepath)))))

(define (%compile-source-to-code str)
  (let ((exprs (read-from-string str)))
    (if (null? exprs)
        #f
        (let loop ((rest exprs) (compiled '()))
          (if (null? rest)
              (cond
                ((null? compiled) (compile-to-code (void)))
                ((null? (cdr compiled)) (compile-to-code (car compiled)))
                (else (compile-to-code (cons 'begin (reverse compiled)))))
              (let ((e (car rest)))
                (cond
                  ;; Top-level begin: splice into rest
                  ((and (pair? e) (eq? (car e) 'begin))
                   (loop (append (cdr e) (cdr rest)) compiled))

                  ;; Macro definition: evaluate immediately in compiler env, then emit its name
                  ((and (pair? e) (or (eq? (car e) 'define-syntax) (eq? (car e) 'define-macro)))
                   (let* ((sig (cadr e))
                          (name (if (pair? sig) (car sig) sig)))
                     (eval e)
                     (loop (cdr rest) (cons (list 'quote name) compiled))))

                  ;; Regular expression: expand macro (if top-level macro call) or expand inner and optimize
                  (else
                   (let ((exp1 (macro-expand-1 e)))
                     (if (car exp1)
                         ;; Top-level macro expanded: loop to handle possible begin or further macro expansion
                         (loop (cons (cdr exp1) (cdr rest)) compiled)
                         ;; Not top-level macro: expand inner macros and optimize AST
                         (let* ((expanded (macro-expand e))
                                (optimized (optimize-ast expanded)))
                           (loop (cdr rest) (cons optimized compiled)))))))))))))


