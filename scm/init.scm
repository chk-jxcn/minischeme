;;; init.scm - Standard Scheme Library for MiniScheme
;;; Provides fundamental higher-order functions, list utilities, and macros.

(define (list . args)
  args)

(define (eqv? a b)
  (if (eq? a b)
      #t
      (if (number? a)
          (if (number? b) (and (eq? (exact? a) (exact? b)) (= a b)) #f)   ; 0 and 0.0 are not eqv?
          (if (char? a)
              (if (char? b) (= (char->integer a) (char->integer b)) #f)
              #f))))

(define (char=? c1 c2) (= (char->integer c1) (char->integer c2)))
(define (char<? c1 c2) (< (char->integer c1) (char->integer c2)))
(define (char>? c1 c2) (> (char->integer c1) (char->integer c2)))
(define (char<=? c1 c2) (<= (char->integer c1) (char->integer c2)))
(define (char>=? c1 c2) (>= (char->integer c1) (char->integer c2)))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caddr x) (car (cdr (cdr x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))

;; Calculate list length with tail recursion
(define (length lst)
  (define (iter l acc)
    (if (null? l)
        acc
        (iter (cdr l) (+ acc 1))))
  (iter lst 0))

;; Reverse a list with tail recursion
(define (reverse lst)
  (define (iter src dst)
    (if (null? src)
        dst
        (iter (cdr src) (cons (car src) dst))))
  (iter lst '()))

;; Append two lists
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

;; Higher-order: map (supports 1 or more lists)
(define (map f lst . rest)
  (define (map1 l)
    (if (null? l) '() (cons (f (car l)) (map1 (cdr l)))))
  (if (null? rest)
      (map1 lst)
      (let ()
        (define (cars l)
          (if (null? l) '() (cons (car (car l)) (cars (cdr l)))))
        (define (cdrs l)
          (if (null? l) '() (cons (cdr (car l)) (cdrs (cdr l)))))
        (define (map-n lists)
          (if (null? (car lists))
              '()
              (cons (apply f (cars lists))
                    (map-n (cdrs lists)))))
        (map-n (cons lst rest)))))

;; Higher-order: for-each
(define (for-each proc lst . rest)
  (define (for-each1 l)
    (if (not (null? l))
        (begin
          (proc (car l))
          (for-each1 (cdr l)))))
  (if (null? rest)
      (for-each1 lst)
      (let ()
        (define (cars l)
          (if (null? l) '() (cons (car (car l)) (cars (cdr l)))))
        (define (cdrs l)
          (if (null? l) '() (cons (cdr (car l)) (cdrs (cdr l)))))
        (define (for-each-n lists)
          (if (not (null? (car lists)))
              (begin
                (apply proc (cars lists))
                (for-each-n (cdrs lists)))))
        (for-each-n (cons lst rest)))))

;; Higher-order: filter
(define (filter pred lst)
  (if (null? lst)
      '()
      (if (pred (car lst))
          (cons (car lst) (filter pred (cdr lst)))
          (filter pred (cdr lst)))))

;; Higher-order: fold-left (tail recursive)
(define (fold-left f acc lst)
  (if (null? lst)
      acc
      (fold-left f (f acc (car lst)) (cdr lst))))

;; Higher-order: fold-right
(define (fold-right f acc lst)
  (if (null? lst)
      acc
      (f (car lst) (fold-right f acc (cdr lst)))))

;; Convenient print procedure
(define (print . args)
  (define (iter lst)
    (if (null? lst)
        (newline)
        (begin
          (display (car lst))
          (if (null? (cdr lst))
              (newline)
              (begin
                (display " ")
                (iter (cdr lst)))))))
  (iter args))

;; =========================================================================
;; Standard Control Flow Macros (R7RS syntax-rules)
;; =========================================================================

(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test body ...)
     (if (not test) (begin body ...)))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and x) x)
    ((and x y ...)
     (if x (and y ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or x) x)
    ((or x y ...)
     (let ((temp x))
       (if temp temp (or y ...))))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...) clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax case
  (syntax-rules (else)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))
    ((case key
       (atom result1 result2 ...))
     (if (eqv? key 'atom)
         (begin result1 result2 ...)))
    ((case key
       (atom result1 result2 ...)
       clause clauses ...)
     (if (eqv? key 'atom)
         (begin result1 result2 ...)
         (case key clause clauses ...)))))


;; List search: memq, memv, member
(define (memq obj lst)
  (if (null? lst)
      #f
      (if (eq? (car lst) obj)
          lst
          (memq obj (cdr lst)))))

(define (memv obj lst)
  (if (null? lst)
      #f
      (if (eqv? (car lst) obj)
          lst
          (memv obj (cdr lst)))))

(define (member obj lst)
  (if (null? lst)
      #f
      (if (equal? (car lst) obj)
          lst
          (member obj (cdr lst)))))

;; Association list: assq, assv, assoc
(define (assq obj alst)
  (if (null? alst)
      #f
      (if (pair? (car alst))
          (if (eq? (car (car alst)) obj)
              (car alst)
              (assq obj (cdr alst)))
          (assq obj (cdr alst)))))

(define (assv obj alst)
  (if (null? alst)
      #f
      (if (pair? (car alst))
          (if (eqv? (car (car alst)) obj)
              (car alst)
              (assv obj (cdr alst)))
          (assv obj (cdr alst)))))

(define (assoc obj alst)
  (if (null? alst)
      #f
      (if (pair? (car alst))
          (if (equal? (car (car alst)) obj)
              (car alst)
              (assoc obj (cdr alst)))
          (assoc obj (cdr alst)))))

;; list-ref
(define (list-ref lst idx)
  (if (= idx 0)
      (car lst)
      (list-ref (cdr lst) (- idx 1))))

;; max & min
(define (max a . rest)
  (define (iter m lst)
    (if (null? lst)
        m
        (iter (if (> (car lst) m) (car lst) m) (cdr lst))))
  (iter a rest))

(define (min a . rest)
  (define (iter m lst)
    (if (null? lst)
        m
        (iter (if (< (car lst) m) (car lst) m) (cdr lst))))
  (iter a rest))

;; =========================================================================
;; functions & dir: Lightweight function listing by category
;; =========================================================================

(define (%category-functions cat-str)
  (if (defined? '%help-names-for-category)
      (%help-names-for-category cat-str)
      (let ((entries (%help-entries-for-category cat-str)))
        (if (null? entries)
            '()
            (map (lambda (e) (string->symbol (car e))) entries)))))

(define (%user-defined-functions)
  (if (defined? '%user-globals)
      (filter (lambda (sym)
                (let ((s (symbol->string sym)))
                  (and (> (string-length s) 0)
                       (not (char=? (string-ref s 0) #\%))
                       (not (memq sym '(functions dir)))
                       (procedure? (%get-global sym)))))
              (%user-globals))
      '()))

(define (%functions-impl raw-target)
  (let* ((target (if (and (pair? raw-target) (eq? (car raw-target) 'quote) (pair? (cdr raw-target)))
                     (cadr raw-target)
                     raw-target))
         (s (cond
              ((symbol? target) (symbol->string target))
              ((string? target) target)
              (else "all"))))
    (cond
      ((or (equal? s "hardware") (equal? s "hw"))
       (%category-functions "Hardware & Peripherals"))

      ((equal? s "math")
       (%category-functions "Math & Numeric"))

      ((or (equal? s "list") (equal? s "lists"))
       (%category-functions "Pairs & Lists"))

      ((or (equal? s "vector") (equal? s "vectors"))
       (%category-functions "Vectors"))

      ((or (equal? s "string") (equal? s "strings") (equal? s "char"))
       (%category-functions "Strings & Characters"))

      ((or (equal? s "predicate") (equal? s "predicates") (equal? s "type") (equal? s "types"))
       (%category-functions "Predicates & Types"))

      ((or (equal? s "io") (equal? s "sys") (equal? s "system"))
       (%category-functions "I/O & System"))

      ((or (equal? s "control") (equal? s "macro") (equal? s "macros"))
       (%category-functions "Control Flow & Macros"))

      ((equal? s "special")
       (%category-functions "Special Forms"))

      ((equal? s "user")
       (%user-defined-functions))

      ((or (equal? s "library") (equal? s "lib"))
       (append (%category-functions "Math & Numeric")
               (%category-functions "Pairs & Lists")
               (%category-functions "Vectors")
               (%category-functions "Strings & Characters")))

      ((or (equal? s "categories") (equal? s "cats"))
       '(hardware math list vector string predicates io control special user library))

      (else
       (let cat-loop ((cats (%help-categories)) (acc '()))
         (if (null? cats)
             (let ((uf (%user-defined-functions)))
               (if (null? uf)
                   (reverse acc)
                   (reverse (cons (cons 'user uf) acc))))
             (let* ((cat (car cats))
                    (fns (%category-functions cat)))
               (cat-loop (cdr cats)
                         (cons (cons (string->symbol cat) fns) acc)))))))))

(define hardware 'hardware)
(define math 'math)
(define user 'user)
(define categories 'categories)
(define library 'library)

(define (functions . rest)
  (%functions-impl (if (null? rest) 'all (car rest))))

(define (dir . rest)
  (%functions-impl (if (null? rest) 'all (car rest))))



