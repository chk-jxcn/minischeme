;;; =========================================================================
;;; help.scm - Interactive Help System in Scheme
;;; =========================================================================

(define *hsep* "============================================================\n")

(define (%print-pad-right str len)
  (display str)
  (let loop ((i (string-length str)))
    (if (< i len)
        (begin (display " ") (loop (+ i 1))))))

(define (%show-help target)
  (if (null? (%help-categories))
      (display "Help system is disabled in this release build.\n")
      (let ((sym (cond
               ((not target) #f)
               ((symbol? target) (symbol->string target))
               ((string? target) target)
               ((and (pair? target) (eq? (car target) 'quote) (pair? (cdr target)))
                (let ((unquoted (cadr target)))
                  (if (symbol? unquoted) (symbol->string unquoted)
                      (if (string? unquoted) unquoted #f))))
               (else #f))))
    (if sym
        (let ((entry (%help-lookup sym)))
          (if entry
              (begin
                (display "\n") (display *hsep*)
                (display "  Name:        ") (display (car entry)) (newline)
                (display "  Syntax:      ") (display (cadr entry)) (newline)
                (display "  Category:    ") (display (caddr entry)) (newline)
                (display "  Description: ") (display (cadddr entry)) (newline)
                (display *hsep*))
              (begin
                (display "No documentation found for '") (display sym) (display "'.\n")
                (display "Type (help) to list all available standard functions.\n"))))
        (begin
          (display "\n") (display *hsep*)
          (display "            MiniScheme Built-in Functions & Reference       \n")
          (display *hsep*)
          (let cat-loop ((cats (%help-categories)))
            (if (not (null? cats))
                (let ((cat (car cats)))
                  (display "\n[") (display cat) (display "]\n")
                  (let entry-loop ((entries (%help-entries-for-category cat)))
                    (if (not (null? entries))
                        (let ((entry (car entries)))
                          (display "  ")
                          (%print-pad-right (cadr entry) 32)
                          (display " - ")
                          (display (cadddr entry))
                          (newline)
                          (entry-loop (cdr entries)))))
                  (cat-loop (cdr cats)))))
          (display "\n------------------------------------------------------------\n")
          (display "  Tip: Type (help 'symbol) or (help symbol) for details.\n")
          (display *hsep*) (newline))))))

(define-syntax help
  (syntax-rules ()
    ((help) (%show-help #f))
    ((help x) (%show-help 'x))))
