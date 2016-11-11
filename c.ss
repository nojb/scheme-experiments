(import (chezscheme))

(define (emit . args)
  (write-char #\tab)
  (apply printf args)
  (newline))

(define (emit-label label)
  (printf "L~a:\n" label))

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
  (not (pair? e)))

(define (primcall? e)
  (and (pair? e) (eq? (car e) 'primcall)))

(define (primcall-op e)
  (cadr e))

(define (primcall-arg1 e)
  (caddr e))

(define (primcall-args e)
  (cddr e))

(define (immediate-rep x)
  (cond
   ((integer? x) (fxlogor (fxsll x fixnum-shift) fixnum-tag))
   ((boolean? x) (fxlogor (fxsll (if x 1 0) boolean-shift) boolean-tag))
   ((char? x) (fxlogor (fxsll (char->integer x) char-shift) char-tag))
   ((null? x) emptylist-tag)
   (else (error 'immediate-rep "unhandled"))))

(define wordsize 8)

(define (emit-expr x si)
  (cond
   ((immediate? x)
    (emit "movq	$~a, %rax" (immediate-rep x)))
   ((primcall? x)
    (case (primcall-op x)
      ((add1)
       (emit-expr (primcall-arg1 x) si)
       (emit "addq	$2, %rax"))
      ((char->integer)
       (emit-expr (primcall-arg1 x) si)
       (emit "shrq	$~a, %rax" (- char-shift fixnum-shift))
       (emit "orq	$~a, %rax" fixnum-tag))
      ((integer->char)
       (emit-expr (primcall-arg1 x) si)
       (emit "shrq	$1, %rax")
       (emit "shlq	$~a, %rax" char-shift)
       (emit "orq	$~a, %rax" char-tag))
      ((integer?)
       (emit-expr (primcall-arg1 x) si)
       (emit "andq	$1, %rax")
       (emit "shlq	$~a, %rax" boolean-shift)
       (emit "orq	$~a, %rax" boolean-tag))
      ((+)
       (let ((args (reverse (primcall-args x))))
         (if (null? args)
             (emit "movq	$1, %rax")
             (let ((x (car args)) (args (cdr args)))
               (emit-expr x si)
               (let loop ((args args) (si si))
                 (if (pair? args)
                     (begin
                       (emit "movq	%rax, ~a(%rsp)" si)
                       (emit-expr (car args) (- si wordsize))
                       (emit "addq	~a(%rsp), %rax" si)
                       (loop (cdr args) si))))
               (emit "subq	$~a, %rax" (length args))))))))))

(define (compile-all x)
  (emit ".text")
  (emit ".globl	_scheme_entry")
  (emit-symbol "scheme_entry")
  (emit-expr x (- wordsize))
  (emit "retq"))

(compile-all (read))
