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

(define (compile-program x)
  (define (immediate-rep x)
    (cond
     ((integer? x) (fxlogor (fxsll x fixnum-shift) fixnum-tag))
     ((boolean? x) (fxlogor (fxsll (if x 1 0) boolean-shift) boolean-tag))
     ((char? x) (fxlogor (fxsll (char->integer x) char-shift) char-tag))))
  (emit "movl	$~a, %eax" (immediate-rep x))
  (emit "retq"))

(define (compile-all x)
  (emit ".text")
  (emit ".globl	_scheme_entry")
  ;; (emit ".type	_scheme_entry, @function")
  (emit-symbol "scheme_entry")
  (compile-program x))

(compile-all (read))
