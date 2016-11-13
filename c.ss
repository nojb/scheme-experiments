(import (chezscheme))

(define (emit . args)
  (write-char #\tab)
  (apply printf args)
  (newline))

(define (emit-label label)
  (printf "~a:\n" label))

(define (emit-symbol s)
  (printf "_~a:\n" s))

(define fixnum-shift 1)
(define fixnum-tag 1)

(define boolean-shift 4)
(define boolean-tag #b0110)

(define char-shift 8)
(define char-tag #b1010)

(define emptylist-tag #b1110)

(define (immediate? e)
  (or (integer? e) (boolean? e) (char? e) (null? e)))

(define (primcall? e)
  (and (pair? e) (eq? (car e) 'primcall)))

(define (primcall-op e)
  (cadr e))

(define (primcall-arg1 e)
  (caddr e))

(define (primcall-arg2 e)
  (cadddr e))

(define (primcall-args e)
  (cddr e))

(define (immediate-rep x)
  (cond
   ((integer? x) (fxlogor (fxsll x fixnum-shift) fixnum-tag))
   ((boolean? x) (fxlogor (fxsll (if x 1 0) boolean-shift) boolean-tag))
   ((char? x) (fxlogor (fxsll (char->integer x) char-shift) char-tag))
   ((null? x) emptylist-tag)
   (else (error 'immediate-rep "unhandled" x))))

(define wordsize 8)

(define (lookup x env)
  (cdr (or (assoc x env) (error 'lookup "not found" x env))))

(define (variable? x)
  (symbol? x))

(define (add-var x si env)
  (cons `(,x . ,si) env))

(define (let? e)
  (and (pair? e) (eq? (car e) 'let)))

(define (rhs e)
  (cadr e))

(define (lhs e)
  (car e))

(define (emit-let bindings body si env)
  (let loop ((bindings bindings) (si si) (env env))
    (cond
     ((null? bindings)
      (emit-expr-list body si env))
     (else
      (let ((binding (car bindings)))
        (emit-expr (rhs binding) si env)
        (emit "movq	%rax, ~a(%rsp)" si)
        (loop (cdr bindings) (- si wordsize) (add-var (lhs binding) si env)))))))

(define (bindings e)
  (cadr e))

(define (body e)
  (cddr e))

(define (if? e)
  (and (pair? e) (eq? (car e) 'if)))

(define (emit-cmpq op1 op2)
  (emit "cmpq	~a, ~a" op1 op2))

(define label-count -1)

(define (unique-label)
  (set! label-count (+ label-count 1))
  (string-append "L" (number->string label-count)))

(define (emit-if test conseq altern si env)
  (let* ((L0 (unique-label)) (L1 (unique-label)))
    (emit-expr test si env)
    (emit "cmpq	$~a, %rax" (immediate-rep #f))
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

(define (test x)
  (cadr x))

(define (conseq x)
  (caddr x))

(define (altern x)
  (cadddr x))

(define cons-tag #x03)

(define (emit-expr x si env)
  (cond
   ((immediate? x)
    (emit "movq	$~a, %rax" (immediate-rep x)))
   ((variable? x)
    (emit "movq	~a(%rsp), %rax" (lookup x env)))
   ((let? x)
    (emit-let (bindings x) (body x) si env))
   ((if? x)
    (emit-if (test x) (conseq x) (altern x) si env))
   ((primcall? x)
    (case (primcall-op x)
      ((add1)
       (emit-expr (primcall-arg1 x) si env)
       (emit "addq	$2, %rax"))
      ((char->integer)
       (emit-expr (primcall-arg1 x) si env)
       (emit "shrq	$~a, %rax" (- char-shift fixnum-shift))
       (emit "orq	$~a, %rax" fixnum-tag))
      ((integer->char)
       (emit-expr (primcall-arg1 x) si env)
       (emit "shrq	$1, %rax")
       (emit "shlq	$~a, %rax" char-shift)
       (emit "orq	$~a, %rax" char-tag))
      ((integer?)
       (emit-expr (primcall-arg1 x) si env)
       (emit "andq	$1, %rax")
       (emit "shlq	$~a, %rax" boolean-shift)
       (emit "orq	$~a, %rax" boolean-tag))
      ((+)
       (let ((args (reverse (primcall-args x))))
         (if (null? args)
             (emit "movq	$1, %rax")
             (let ((x (car args)) (args (cdr args)))
               (emit-expr x si env)
               (let loop ((args args) (si si))
                 (if (pair? args)
                     (begin
                       (emit "movq	%rax, ~a(%rsp)" si)
                       (emit-expr (car args) (- si wordsize) env)
                       (emit "addq	~a(%rsp), %rax" si)
                       (loop (cdr args) si))))
               (emit "subq	$~a, %rax" (length args))))))
      ((cons)
       (let ((a (primcall-arg1 x)) (b (primcall-arg2 x)))
         (emit-expr a si env)
         (emit "movq	%rax, ~a(%rsp)" si)
         (emit-expr b (- si wordsize) env)
         (emit "movb	$~a, 0(%rsi)" cons-tag)
         (emit "movq	%rax, ~a(%rsi)" (* 2 wordsize))
         (emit "movq	~a(%rsp), %rax" si)
         (emit "movq	%rax, ~a(%rsi)" wordsize)
         (emit "leaq	~a(%rsi), %rax" wordsize)
         (emit "addq	$~a, %rsi" (* 3 wordsize))))
      ((car)
       (emit-expr (primcall-arg1 x) si env)
       (emit "movq	0(%rax), %rax"))
      ((cdr)
       (emit-expr (primcall-arg1 x) si env)
       (emit "movq	~a(%rax), %rax" wordsize))))))

(define (compile-all x)
  (emit ".text")
  (emit ".globl	_scheme_entry")
  (emit-symbol "scheme_entry")
  (emit-expr x (- wordsize) '())
  (emit "retq"))

(compile-all (read))
