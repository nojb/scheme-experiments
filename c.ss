(import (chezscheme))

(define (emit . args)
  (write-char #\tab)
  (apply printf args)
  (newline))

(define (emit-label label)
  (printf "~a:\n" label))

(define (emit-symbol s)
  (printf "_~a:\n" s))

;; (define fixnum-shift 1)
;; (define fixnum-tag 1)

;; (define boolean-shift 4)
;; (define boolean-tag #b0110)

;; (define char-shift 8)
;; (define char-tag #b1010)

;; (define emptylist-tag #b1110)

(define wordsize 8)

(define (lookup x env)
  (cdr (or (assoc x env) (error 'lookup "not found" x env))))

(define (variable? x)
  (symbol? x))

(define (add-var x si env)
  (cons `(,x . ,si) env))

(define (emit-let bindings body si env)
  (let loop ((bindings bindings) (si si) (env env))
    (cond
      ((null? bindings)
        (emit-expr-list body si env))
      (else
        (let ((binding (car bindings)))
          (emit-expr (cadr binding) si env)
          (emit "movq	%rax, ~a(%rsp)" si)
          (loop (cdr bindings) (- si wordsize)
            (add-var (car binding) si env)))))))

(define label-count -1)

(define (unique-label)
  (set! label-count (+ label-count 1))
  (string-append "L" (number->string label-count)))

(define (emit-if test conseq altern si env)
  (let* ((L0 (unique-label)) (L1 (unique-label)))
    (emit-expr test si env)
    (emit "cmpq	$0, %rax")
    (emit "je	~a" L0)
    (emit-expr conseq si env)
    (emit "jmp	~a" L1)
    (emit-label L0)
    (emit-expr altern si env)
    (emit-label L1)))

(define (emit-expr-list x si env)
  (if (pair? x)
    (begin
      (emit-expr (car x) si env)
      (emit-expr-list (cdr x) si env))))

(define (emit-expr x si env)
  (cond
    ((integer? x) (emit "movq	$~a, %rax" x))
    ((boolean? x) (emit "movq	$~a, %rax" (if x 1 0)))
    ((char? x) (emit "movq	$~a, %rax" (char->integer x)))
    ((symbol? x) (emit "movq	~a(%rsp), %rax" (lookup x env)))
    ((pair? x)
      (case (car x)
        ((set!)
          (emit-expr (caddr x) si env)
          (emit "movq	%rax, ~a(%rsp)" (lookup (cadr x) env)))
        ((let)
          (emit-let (cadr x) (cddr x) si env))
        ((if)
          (emit-if (cadr x) (caddr x) (cadddr x) si env))
        ((+)
          (emit-expr (cadr x) si env)
          (emit "pushq	%rax")
          (emit-expr (caddr x) si env)
          (emit "addq	(%rsp), %rax")
          (emit "popq"))))))

(define (compile-all x)
  (emit ".text")
  (emit ".globl	_oberon_entry")
  (emit-symbol "oberon_entry")
  (emit-expr x (- wordsize) '())
  (emit "retq"))

(compile-all (read))
