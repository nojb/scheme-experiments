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

(define (immediate-rep x)
  (cond
   ((integer? x) (fxlogor (fxsll x fixnum-shift) fixnum-tag))
   ((boolean? x) (fxlogor (fxsll (if x 1 0) boolean-shift) boolean-tag))
   ((char? x) (fxlogor (fxsll (char->integer x) char-shift) char-tag))
   ((null? x) emptylist-tag)
   (else (error 'immediate-rep "unhandled"))))

(define (emit-expr x)
  (cond
   ((immediate? x)
    (emit "movq	$~a, %rax" (immediate-rep x)))
   ((primcall? x)
    (case (primcall-op x)
      ((add1)
       (emit-expr (primcall-arg1 x))
       (emit "addq	$2, %rax"))
      ((char->integer)
       (emit-expr (primcall-arg1 x))
       (emit "shrq	$~a, %rax" (- char-shift fixnum-shift))
       (emit "orq	$~a, %rax" fixnum-tag))))))

(define (compile-all x)
  (emit ".text")
  (emit ".globl	_scheme_entry")
  (emit-symbol "scheme_entry")
  (emit-expr x)
  (emit "retq"))

(compile-all (read))
